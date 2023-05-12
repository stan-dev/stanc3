@Library('StanUtils')
import org.stan.Utils

utils = new org.stan.Utils()

def skipExpressionTests = false
def skipRemainingStages = false
def skipCompileTests = false
def skipRebuildingBinaries = false
def skipCompileTestsAtO1 = false

/* Functions that runs a sh command and returns the stdout */
def runShell(String command){
    def output = sh (returnStdout: true, script: "${command}").trim()
    return "${output}"
}

def tagName() {
    if (env.TAG_NAME) {
        env.TAG_NAME
    } else if (env.BRANCH_NAME == 'master') {
        'nightly'
    } else {
        'unknown-tag'
    }
}

def runPerformanceTests(String testsPath, String stancFlags = ""){
    unstash 'ubuntu-exe'

    sh """
        git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
    """

    writeFile(file:"performance-tests-cmdstan/cmdstan/make/local", text:"CXX=${CXX}")

    utils.checkout_pr("cmdstan", "performance-tests-cmdstan/cmdstan", params.cmdstan_pr)
    utils.checkout_pr("stan", "performance-tests-cmdstan/cmdstan/stan", params.stan_pr)
    utils.checkout_pr("math", "performance-tests-cmdstan/cmdstan/stan/lib/stan_math", params.math_pr)

    sh """
        cd performance-tests-cmdstan
        mkdir cmdstan/bin
        cp ../bin/stanc cmdstan/bin/linux-stanc
        cd cmdstan; make clean-all;
    """

    if (stancFlags?.trim()) {
        sh "cd performance-tests-cmdstan/cmdstan && echo 'STANCFLAGS= $stancFlags' >> make/local"
    }

    sh """
        cd performance-tests-cmdstan/cmdstan
        echo 'O=0' >> make/local
        make -j${env.PARALLEL} build; cd ..
        ./runPerformanceTests.py -j${env.PARALLEL} --runs=0 ${testsPath}
    """
}

pipeline {
    agent none
    parameters {
        booleanParam(name:"skip_end_to_end", defaultValue: false, description:"Skip end-to-end tests ")
        booleanParam(name:"skip_compile_O1", defaultValue: false, description:"Skip compile tests that run with STANCFLAGS = --O1")
        string(defaultValue: 'develop', name: 'cmdstan_pr',
               description: "CmdStan PR to test against. Will check out this PR in the downstream Stan repo.")
        string(defaultValue: 'develop', name: 'stan_pr',
               description: "Stan PR to test against. Will check out this PR in the downstream Stan repo.")
        string(defaultValue: 'develop', name: 'math_pr',
               description: "Math PR to test against. Will check out this PR in the downstream Math repo.")
        string(defaultValue: '', name: 'stanc_flags',
               description: "Pass STANCFLAGS to make/local, default none")
    }
    options {
        parallelsAlwaysFailFast()
        skipDefaultCheckout()
    }
    environment {
        CXX = 'clang++-6.0'
        PARALLEL = 4
        GIT_AUTHOR_NAME = 'Stan Jenkins'
        GIT_AUTHOR_EMAIL = 'mc.stanislaw@gmail.com'
        GIT_COMMITTER_NAME = 'Stan Jenkins'
        GIT_COMMITTER_EMAIL = 'mc.stanislaw@gmail.com'
    }
    stages {
        stage('Kill previous builds') {
            when {
                not { branch 'develop' }
                not { branch 'master' }
                not { branch 'downstream_tests' }
            }
            steps { script { utils.killOldBuilds() } }
        }
        stage('Verify changes') {
            agent {
                docker {
                    image 'stanorg/stanc3:debianfi'
                    args "--entrypoint=\'\'"
                    label 'linux'
                }
            }
            steps {
                script {
                    retry(3) {
                        checkout([
                          $class: 'GitSCM',
                          branches: scm.branches,
                          extensions: [[$class: 'CloneOption', noTags: false]],
                          userRemoteConfigs: scm.userRemoteConfigs,
                        ])
                    }
                    sh 'git clean -xffd'

                    runShell """
                        eval \$(opam env)
                        dune subst
                    """

                    stash 'Stanc3Setup'

                    def stanMathSigs = ['test/integration/signatures/stan_math_signatures.t'].join(" ")
                    skipExpressionTests = utils.verifyChanges(stanMathSigs, "master")

                    def runTestPaths = ['src', 'test/integration/good', 'test/stancjs'].join(" ")
                    skipRemainingStages = utils.verifyChanges(runTestPaths, "master")

                    def compileTests = ['test/integration/good'].join(" ")
                    skipCompileTests = utils.verifyChanges(compileTests, "master")

                    def compileTestsAtO1 = ['test/integration/good/compiler-optimizations'].join(" ")
                    skipCompileTestsAtO1 = utils.verifyChanges(compileTestsAtO1, "master")

                    def sourceCodePaths = ['src', 'Jenkinsfile'].join(" ")
                    skipRebuildingBinaries = utils.verifyChanges(sourceCodePaths, "master")
                }
            }
            post { always { runShell("rm -rf ./*") }}
        }

        stage("Build") {
            when {
                beforeAgent true
                expression {
                    !skipRemainingStages
                }
            }
            agent {
                docker {
                    image 'stanorg/stanc3:debianfi'
                    //Forces image to ignore entrypoint
                    args "--entrypoint=\'\'"
                    label 'linux'
                }
            }
            steps {
                unstash "Stanc3Setup"
                runShell("""
                    eval \$(opam env)
                    dune build @install
                """)

                sh "mkdir -p bin && mv _build/default/src/stanc/stanc.exe bin/stanc"
                stash name:'ubuntu-exe', includes:'bin/stanc, notes/working-models.txt'
            }
            post { always { runShell("rm -rf ./*") }}
        }

        stage("Code formatting") {
            when {
                beforeAgent true
                expression {
                    !skipRemainingStages
                }
            }
            agent {
                docker {
                    image 'stanorg/stanc3:debianfi'
                    //Forces image to ignore entrypoint
                    args "--entrypoint=\'\'"
                }
            }
            steps {
                unstash "Stanc3Setup"
                sh """
                    eval \$(opam env)
                    make format  ||
                    (
                        set +x &&
                        echo "The source code was not formatted. Please run 'make format; dune promote' and push the changes." &&
                        echo "Please consider installing the pre-commit git hook for formatting with the above command." &&
                        echo "Our hook can be installed with bash ./scripts/hooks/install_hooks.sh" &&
                        exit 1;
                    )
                """
            }
            post { always { runShell("rm -rf ./*") }}
        }

        stage("OCaml tests") {
            when {
                beforeAgent true
                expression {
                    !skipRemainingStages
                }
            }
            parallel {
                stage("Dune tests") {
                    agent {
                        docker {
                            image 'stanorg/stanc3:debianfi'
                            //Forces image to ignore entrypoint
                            args "--entrypoint=\'\'"
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/dune-tests"){
                            unstash "Stanc3Setup"
                            runShell("""
                                eval \$(opam env)
                                BISECT_FILE=\$(pwd)/bisect dune runtest --instrument-with bisect_ppx --force --root=.
                            """)

                            sh """
                                eval \$(opam env)
                                bisect-ppx-report summary --expect src/ --do-not-expect src/stancjs/
                                bisect-ppx-report coveralls coverage.json --service-name jenkins --service-job-id $BUILD_ID --expect src/ --do-not-expect src/stancjs/
                            """

                            withCredentials([usernamePassword(credentialsId: 'stan-stanc3-codecov-token', usernameVariable: 'DUMMY_USERNAME', passwordVariable: 'CODECOV_TOKEN')]) {
                                sh """
                                    curl -Os https://uploader.codecov.io/v0.3.2/linux/codecov

                                    chmod +x codecov
                                    ./codecov -v -C \$(git rev-parse HEAD)
                                """
                            }
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/dune-tests/*") }}
                }
                stage("stancjs tests") {
                    agent {
                        docker {
                            image 'stanorg/stanc3:debianfi'
                            //Forces image to ignore entrypoint
                            args "--entrypoint=\'\'"
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/stancjs-tests"){
                            unstash "Stanc3Setup"
                            runShell("""
                                eval \$(opam env)
                                dune build @runjstest --root=.
                            """)
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/stancjs-tests/*") }}
                }
            }
        }

        stage("CmdStan & Math tests") {
            parallel {

                stage("Compile tests - good") {
                    when {
                        beforeAgent true
                        expression {
                            !skipCompileTests
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/ci:gpu'
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/compile-tests-good"){
                            unstash "Stanc3Setup"
                            script {
                                runPerformanceTests("../test/integration/good", params.stanc_flags)
                            }

                            xunit([GoogleTest(
                                deleteOutputFiles: false,
                                failIfNotNew: true,
                                pattern: 'performance-tests-cmdstan/performance.xml',
                                skipNoTestFiles: false,
                                stopProcessingIfError: false)
                            ])
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/compile-tests-good/*") }}
                }

                stage("Compile tests - example-models") {
                    when {
                        beforeAgent true
                        expression {
                            !skipCompileTests
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/ci:gpu'
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/compile-tests-example"){
                            script {
                                unstash "Stanc3Setup"
                                runPerformanceTests("example-models", params.stanc_flags)
                            }

                            xunit([GoogleTest(
                                deleteOutputFiles: false,
                                failIfNotNew: true,
                                pattern: 'performance-tests-cmdstan/performance.xml',
                                skipNoTestFiles: false,
                                stopProcessingIfError: false)
                            ])
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/compile-tests-example/*") }}
                }

                stage("Compile tests - good at O=1") {
                    when {
                        beforeAgent true
                        allOf {
                          expression {
                            !skipCompileTestsAtO1
                          }
                          expression {
                            !params.skip_compile_O1
                          }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/ci:gpu'
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/compile-good-O1"){
                            unstash "Stanc3Setup"
                            script {
                                runPerformanceTests("../test/integration/good", "--O1")
                            }

                            xunit([GoogleTest(
                                deleteOutputFiles: false,
                                failIfNotNew: true,
                                pattern: 'performance-tests-cmdstan/performance.xml',
                                skipNoTestFiles: false,
                                stopProcessingIfError: false)
                            ])
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/compile-good-O1/*") }}
                }

                stage("Compile tests - example-models at O=1") {
                    when {
                        beforeAgent true
                        allOf {
                          expression {
                            !skipCompileTestsAtO1
                          }
                          expression {
                            !params.skip_compile_O1
                          }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/ci:gpu'
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/compile-example-O1"){
                            script {
                                unstash "Stanc3Setup"
                                runPerformanceTests("example-models", "--O1")
                            }

                            xunit([GoogleTest(
                                deleteOutputFiles: false,
                                failIfNotNew: true,
                                pattern: 'performance-tests-cmdstan/performance.xml',
                                skipNoTestFiles: false,
                                stopProcessingIfError: false)
                            ])
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/compile-example-O1/*") }}
                }

                stage("Model end-to-end tests") {
                    when {
                        beforeAgent true
                        allOf {
                         expression {
                            !skipCompileTests
                         }
                         expression {
                            !params.skip_end_to_end
                         }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/ci:gpu'
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/compile-end-to-end"){
                            script {
                                unstash "Stanc3Setup"
                                unstash 'ubuntu-exe'
                                sh """
                                    git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
                                """
                                utils.checkout_pr("cmdstan", "performance-tests-cmdstan/cmdstan", params.cmdstan_pr)
                                utils.checkout_pr("stan", "performance-tests-cmdstan/cmdstan/stan", params.stan_pr)
                                utils.checkout_pr("math", "performance-tests-cmdstan/cmdstan/stan/lib/stan_math", params.math_pr)
                                sh """
                                    cd performance-tests-cmdstan
                                    git show HEAD --stat
                                    echo "example-models/regression_tests/mother.stan" > all.tests
                                    cat known_good_perf_all.tests >> all.tests
                                    echo "" >> all.tests
                                    cat shotgun_perf_all.tests >> all.tests
                                    cat all.tests
                                    echo "CXXFLAGS+=-march=core2" > cmdstan/make/local
                                    echo "PRECOMPILED_HEADERS=false" >> cmdstan/make/local
                                    cd cmdstan; make clean-all; git show HEAD --stat; cd ..
                                    CXX="${CXX}" ./compare-compilers.sh "--tests-file all.tests --num-samples=10" "\$(readlink -f ../bin/stanc)"
                                """
                            }

                            xunit([GoogleTest(
                                deleteOutputFiles: false,
                                failIfNotNew: true,
                                pattern: 'performance-tests-cmdstan/performance.xml',
                                skipNoTestFiles: false,
                                stopProcessingIfError: false)
                            ])

                            archiveArtifacts 'performance-tests-cmdstan/performance.xml'

//                             perfReport modePerformancePerTestCase: true,
//                                 sourceDataFiles: 'performance-tests-cmdstan/performance.xml',
//                                 modeThroughput: false,
//                                 excludeResponseTime: true,
//                                 errorFailedThreshold: 100,
//                                 errorUnstableThreshold: 100
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/compile-end-to-end/*") }}
                }
                stage("Model end-to-end tests at O=1") {
                    when {
                        beforeAgent true
                        allOf {
                         expression {
                            !skipCompileTests
                         }
                         expression {
                            !params.skip_end_to_end
                         }
                         expression {
                            !skipCompileTestsAtO1
                         }
                         expression {
                            !params.skip_compile_O1
                         }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/ci:gpu'
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/compile-end-to-end-O=1"){
                            script {
                                unstash "Stanc3Setup"
                                unstash 'ubuntu-exe'
                                sh """
                                    git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
                                """
                                utils.checkout_pr("cmdstan", "performance-tests-cmdstan/cmdstan", params.cmdstan_pr)
                                utils.checkout_pr("stan", "performance-tests-cmdstan/cmdstan/stan", params.stan_pr)
                                utils.checkout_pr("math", "performance-tests-cmdstan/cmdstan/stan/lib/stan_math", params.math_pr)
                                sh """
                                    cd performance-tests-cmdstan
                                    git show HEAD --stat
                                    echo "example-models/regression_tests/mother.stan" > all.tests
                                    cat known_good_perf_all.tests >> all.tests
                                    echo "" >> all.tests
                                    cat shotgun_perf_all.tests >> all.tests
                                    cat all.tests
                                    echo "CXXFLAGS+=-march=core2" > cmdstan/make/local
                                    echo "PRECOMPILED_HEADERS=false" >> cmdstan/make/local
                                    cd cmdstan; make clean-all; git show HEAD --stat; cd ..
                                    CXX="${CXX}" ./compare-optimizer.sh "--tests-file all.tests --num-samples=10" "--O1" "\$(readlink -f ../bin/stanc)"
                                """
                            }

                            xunit([GoogleTest(
                                deleteOutputFiles: false,
                                failIfNotNew: true,
                                pattern: 'performance-tests-cmdstan/performance.xml',
                                skipNoTestFiles: false,
                                stopProcessingIfError: false)
                            ])

                            archiveArtifacts 'performance-tests-cmdstan/performance.xml'

//                             perfReport modePerformancePerTestCase: true,
//                                 sourceDataFiles: 'performance-tests-cmdstan/performance.xml',
//                                 modeThroughput: false,
//                                 excludeResponseTime: true,
//                                 errorFailedThreshold: 100,
//                                 errorUnstableThreshold: 100
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/compile-end-to-end-O=1/*") }}
                }
                stage('Math functions expressions test') {
                    when {
                        beforeAgent true
                        allOf {
                            expression {
                                !skipRemainingStages
                            }
                            expression {
                                !skipExpressionTests
                            }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/ci:gpu'
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/compile-expressions"){
                            unstash "Stanc3Setup"
                            unstash 'ubuntu-exe'
                            script {
                                sh """
                                    git clone --recursive https://github.com/stan-dev/math.git
                                """
                                utils.checkout_pr("math", "math", params.math_pr)
                                sh """
                                    cp bin/stanc math/test/expressions/stanc
                                """

                                dir("math") {
                                    sh """
                                        echo O=0 >> make/local
                                        echo "CXX=${env.CXX} -Werror " >> make/local
                                    """
                                    withEnv(['PATH+TBB=./lib/tbb']) {
                                        try { sh "./runTests.py -j${env.PARALLEL} test/expressions" }
                                        finally { junit 'test/**/*.xml' }
                                    }
                                }
                            }
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/compile-expressions/*") }}
                }
            }
        }


        stage('Build binaries') {
            parallel {
                stage("Build & test Mac OS X binary") {
                    when {
                        beforeAgent true
                        expression { !skipRebuildingBinaries }
                    }
                    agent { label 'osx && !m1' }
                    steps {
                        dir("${env.WORKSPACE}/osx"){
                            unstash "Stanc3Setup"
                            withEnv(['SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX10.11.sdk', 'MACOSX_DEPLOYMENT_TARGET=10.11']) {
                                runShell("""
                                    export PATH=/Users/jenkins/brew/bin:\$PATH
                                    eval \$(opam env --switch=/Users/jenkins/.opam/4.12.0-mac10.11 --set-switch)
                                    opam update || true
                                    bash -x scripts/install_build_deps.sh
                                    dune build @install --root=.
                                """)
                            }
                            sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/mac-stanc"
                            stash name:'mac-exe', includes:'bin/*'
                        }
                    }
                    post { always { runShell("rm -rf ${env.WORKSPACE}/osx/*") }}
                }

                stage("Build stanc.js") {
                    when {
                        beforeAgent true
                        expression {
                            !skipRebuildingBinaries
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/stanc3:debianfi'
                            //Forces image to ignore entrypoint
                            args "--entrypoint=\'\'"
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/stancjs"){
                            unstash "Stanc3Setup"
                            runShell("""
                                eval \$(opam env)
                                dune build --root=. --profile release src/stancjs
                            """)
                            sh "mkdir -p bin && mv `find _build -name stancjs.bc.js` bin/stanc.js"
                            sh "mv `find _build -name index.html` bin/load_stanc.html"
                            stash name:'js-exe', includes:'bin/*'
                        }
                    }
                    post {always { runShell("rm -rf ${env.WORKSPACE}/stancjs/*")}}
                }

                stage("Build & test a static Linux binary") {
                    when {
                        beforeAgent true
                        expression {
                            !skipRebuildingBinaries
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/stanc3:staticfi'
                            //Forces image to ignore entrypoint
                            args "--entrypoint=\'\'"
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/linux"){
                            unstash "Stanc3Setup"
                            runShell("""
                                eval \$(opam env)
                                dune build @install --profile static --root=.
                            """)
                            sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-stanc"
                            stash name:'linux-exe', includes:'bin/*'
                        }
                    }
                    post {always { runShell("rm -rf ${env.WORKSPACE}/linux/*")}}
                }

                stage("Build & test a static Linux mips64el binary") {
                    when {
                        beforeAgent true
                        allOf {
                            expression { !skipRebuildingBinaries }
                            anyOf { buildingTag(); branch 'master' }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/stanc3:staticfi'
                            //Forces image to ignore entrypoint
                            args "--group-add=987 --group-add=988 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/linux-mips64el"){
                            unstash "Stanc3Setup"
                            sh "bash -x scripts/build_multiarch_stanc3.sh mips64el"

                            sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-mips64el-stanc"

                            stash name:'linux-mips64el-exe', includes:'bin/*'
                        }
                    }
                    post {always { runShell("rm -rf ${env.WORKSPACE}/linux-mips64el/*")}}
                }

                stage("Build & test a static Linux ppc64el binary") {
                    when {
                        beforeAgent true
                        allOf {
                            expression { !skipRebuildingBinaries }
                            anyOf { buildingTag(); branch 'master' }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/stanc3:staticfi'
                            //Forces image to ignore entrypoint
                            args "--group-add=987 --group-add=988 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/linux-ppc64el"){
                            unstash "Stanc3Setup"
                            sh "bash -x scripts/build_multiarch_stanc3.sh ppc64el"
                            sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-ppc64el-stanc"
                            stash name:'linux-ppc64el-exe', includes:'bin/*'
                        }
                    }
                    post {always { runShell("rm -rf ${env.WORKSPACE}/linux-ppc64el/*")}}
                }

                stage("Build & test a static Linux s390x binary") {
                    when {
                        beforeAgent true
                        allOf {
                            expression { !skipRebuildingBinaries }
                            anyOf { buildingTag(); branch 'master' }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/stanc3:staticfi'
                            //Forces image to ignore entrypoint
                            args "--group-add=987 --group-add=988 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                            label 'linux'
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/linux-s390x"){
                            unstash "Stanc3Setup"
                            sh "bash -x scripts/build_multiarch_stanc3.sh s390x"
                            sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-s390x-stanc"
                            stash name:'linux-s390x-exe', includes:'bin/*'
                        }
                    }
                    post {always { runShell("rm -rf ${env.WORKSPACE}/linux-s390x/*")}}
                }

                stage("Build & test a static Linux arm64 binary") {
                    when {
                        beforeAgent true
                        allOf {
                            expression { !skipRebuildingBinaries }
                            anyOf { buildingTag(); branch 'master' }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/stanc3:staticfi'
                            //Forces image to ignore entrypoint
                            label 'linux'
                            args "--group-add=987 --group-add=988 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/linux-arm64"){
                            unstash "Stanc3Setup"
                            sh "bash -x scripts/build_multiarch_stanc3.sh arm64"
                            sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-arm64-stanc"
                            stash name:'linux-arm64-exe', includes:'bin/*'
                        }
                    }
                    post {always { runShell("rm -rf ${env.WORKSPACE}/linux-arm64/*")}}
                }

                stage("Build & test a static Linux armhf binary") {
                    when {
                        beforeAgent true
                        allOf {
                            expression { !skipRebuildingBinaries }
                            anyOf { buildingTag(); branch 'master' }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/stanc3:staticfi'
                            //Forces image to ignore entrypoint
                            label 'linux'
                            args "--group-add=987 --group-add=988 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/linux-armhf"){
                            unstash "Stanc3Setup"
                            sh "bash -x scripts/build_multiarch_stanc3.sh armhf"
                            sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-armhf-stanc"
                            stash name:'linux-armhf-exe', includes:'bin/*'
                        }
                    }
                    post {always { runShell("rm -rf ${env.WORKSPACE}/linux-armhf/*")}}
                }

                stage("Build & test a static Linux armel binary") {
                    when {
                        beforeAgent true
                        allOf {
                            expression { !skipRebuildingBinaries }
                            anyOf { buildingTag(); branch 'master' }
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/stanc3:staticfi'
                            //Forces image to ignore entrypoint
                            label 'linux'
                            args "--group-add=987 --group-add=988 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/linux-armel"){
                            unstash "Stanc3Setup"
                            sh "bash -x scripts/build_multiarch_stanc3.sh armel"
                            sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-armel-stanc"
                            stash name:'linux-armel-exe', includes:'bin/*'
                        }
                    }
                    post {always { runShell("rm -rf ${env.WORKSPACE}/linux-armel/*")}}
                }

                // Cross compiling for windows on debian
                stage("Build & test static Windows binary") {
                    when {
                        beforeAgent true
                        expression {
                            !skipRebuildingBinaries
                        }
                    }
                    agent {
                        docker {
                            image 'stanorg/stanc3:debian-windowsfi'
                            label 'linux'
                            //Forces image to ignore entrypoint
                            args "--group-add=987 --group-add=988 --entrypoint=\'\'"
                        }
                    }
                    steps {
                        dir("${env.WORKSPACE}/windows"){
                            unstash "Stanc3Setup"
                            runShell("""
                                eval \$(opam env)
                                dune build -x windows --root=.
                            """)
                            sh "mkdir -p bin && mv _build/default.windows/src/stanc/stanc.exe bin/windows-stanc"
                            stash name:'windows-exe', includes:'bin/*'
                        }
                    }
                    post {always { runShell("rm -rf ${env.WORKSPACE}/windows/*")}}
                }

            }
        }

        stage("Release tag and publish binaries") {
            when {
                beforeAgent true
                allOf {
                    expression { !skipRemainingStages }
                    expression { !skipRebuildingBinaries }
                    anyOf { buildingTag(); branch 'master' }
                }
            }
            agent {
                docker {
                    image 'stanorg/ci:gpu'
                    label 'linux'
                }
            }
            environment { GITHUB_TOKEN = credentials('6e7c1e8f-ca2c-4b11-a70e-d934d3f6b681') }
            steps {
                retry(3) {
                    unstash 'windows-exe'
                    unstash 'linux-exe'
                    unstash 'mac-exe'
                    unstash 'linux-mips64el-exe'
                    unstash 'linux-ppc64el-exe'
                    unstash 'linux-s390x-exe'
                    unstash 'linux-arm64-exe'
                    unstash 'linux-armhf-exe'
                    unstash 'linux-armel-exe'
                    unstash 'js-exe'

                    runShell("""
                        wget https://github.com/tcnksm/ghr/releases/download/v0.12.1/ghr_v0.12.1_linux_amd64.tar.gz
                        tar -zxvpf ghr_v0.12.1_linux_amd64.tar.gz
                        ./ghr_v0.12.1_linux_amd64/ghr -r stanc3 -u stan-dev -recreate ${tagName()} bin/
                    """)
                }
            }
            post {
                failure {
                    archiveArtifacts 'bin/*'
                }
            }
        }

        stage('Upload odoc') {
            when {
                beforeAgent true
                branch 'master'
            }
            options { skipDefaultCheckout(true) }
            agent {
                docker {
                    image 'stanorg/stanc3:staticfi'
                    label 'linux'
                    //Forces image to ignore entrypoint
                    args "--entrypoint=\'\'"
                }
            }
            steps {
                retry(3) {
                    checkout([$class: 'GitSCM',
                        branches: [[name: '*/master'], [name: '*/gh-pages']],
                        doGenerateSubmoduleConfigurations: false,
                        extensions: [],
                        submoduleCfg: [],
                        userRemoteConfigs: [[url: "https://github.com/stan-dev/stanc3.git", credentialsId: 'a630aebc-6861-4e69-b497-fd7f496ec46b']]]
                    )
                }

                // Checkout gh-pages as a test so we build docs from this branch
                runShell("""
                    git remote set-branches --add origin gh-pages || true
                    git checkout --track origin/gh-pages || true
                    git remote set-branches --add origin master || true
                    git checkout origin/master || true
                """)

                // Build docs
                runShell("""
                     eval \$(opam env)
                     dune build @doc
                """)

                // Copy static assets to doc
                runShell("""
                    mkdir -p doc
                    cp -r ./_build/default/_doc/_html/* doc/
                """)

                // Commit changes to the repository gh-pages branch
                withCredentials([usernamePassword(credentialsId: 'a630aebc-6861-4e69-b497-fd7f496ec46b', usernameVariable: 'GIT_USERNAME', passwordVariable: 'GIT_PASSWORD')]) {
                    sh """#!/bin/bash
                        set -e

                        git checkout --detach
                        git branch -D gh-pages
                        git push https://${GIT_USERNAME}:${GIT_PASSWORD}@github.com/stan-dev/stanc3.git :gh-pages

                        git checkout --orphan gh-pages
                        git add -f doc
                        git commit -m "auto generated docs from Jenkins"
                        git subtree push --prefix doc/ https://${GIT_USERNAME}:${GIT_PASSWORD}@github.com/stan-dev/stanc3.git gh-pages
                        ls -A1 | xargs rm -rf
                    """
                }

            }
        }

    }
    post {
       always {
          script {
            utils.mailBuildResults()
          }
        }
    }
}
