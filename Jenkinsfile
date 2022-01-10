@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

def skipExpressionTests = false
def skipRemainingStages = false
def skipCompileTests = false
def skipRebuildingBinaries = false
def buildingAgentARM = "linux"

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

pipeline {
    agent none
    parameters {
        booleanParam(name:"skip_end_to_end", defaultValue: false, description:"Skip end-to-end tests ")
        string(defaultValue: 'develop', name: 'cmdstan_pr',
               description: "CmdStan PR to test against. Will check out this PR in the downstream Stan repo.")
        string(defaultValue: 'develop', name: 'stan_pr',
               description: "Stan PR to test against. Will check out this PR in the downstream Stan repo.")
        string(defaultValue: 'develop', name: 'math_pr',
               description: "Math PR to test against. Will check out this PR in the downstream Math repo.")        
    }
    options {parallelsAlwaysFailFast()}
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
            agent { label 'linux' }
            steps {
                script {
                    retry(3) { checkout scm }
                    sh 'git clean -xffd'

                    def stanMathSigs = ['test/integration/signatures/stan_math_signatures.t'].join(" ")
                    skipExpressionTests = utils.verifyChanges(stanMathSigs)

                    def runTestPaths = ['src', 'test/integration/good', 'test/stancjs'].join(" ")
                    skipRemainingStages = utils.verifyChanges(runTestPaths)

                    def compileTests = ['test/integration/good'].join(" ")
                    skipCompileTests = utils.verifyChanges(compileTests)

                    def sourceCodePaths = ['src'].join(" ")
                    skipRebuildingBinaries = utils.verifyChanges(sourceCodePaths)

                    if (buildingTag()) {
                        buildingAgentARM = "arm-ec2"
                    }
                }
            }
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
                    image 'stanorg/stanc3:debian'
                    //Forces image to ignore entrypoint
                    args "-u root --entrypoint=\'\'"
                }
            }
            steps {
                sh 'printenv'
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
                    image 'stanorg/stanc3:debian'
                    //Forces image to ignore entrypoint
                    args "-u root --entrypoint=\'\'"
                }
            }
            steps {
                sh 'printenv'
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
                            image 'stanorg/stanc3:debian'
                            //Forces image to ignore entrypoint
                            args "-u root --entrypoint=\'\'"
                        }
                    }
                    steps {
                        sh 'printenv'
                        runShell("""
                            eval \$(opam env)
                            dune runtest
                        """)
                    }
                    post { always { runShell("rm -rf ./*") }}
                }
                stage("stancjs tests") {
                    agent {
                        docker {
                            image 'stanorg/stanc3:debian'
                            //Forces image to ignore entrypoint
                            args "-u root --entrypoint=\'\'"
                        }
                    }
                    steps {
                        sh 'printenv'
                        runShell("""
                            eval \$(opam env)
                            dune build @runjstest
                        """)
                    }
                    post { always { runShell("rm -rf ./*") }}
                }
            }
        }
        stage("CmdStan & Math tests") {
            parallel {
                stage("Compile tests") {
                    when {
                        beforeAgent true
                        expression {
                            !skipCompileTests
                        }
                    }
                    agent { label 'linux' }
                    steps {
                        script {
                            unstash 'ubuntu-exe'
                            sh """
                                git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
                            """

                            writeFile(file:"performance-tests-cmdstan/cmdstan/make/local",
                                    text:"O=0\nCXX=${CXX}")
                            
                            utils.checkout_pr("cmdstan", "performance-tests-cmdstan/cmdstan", params.cmdstan_pr)
                            utils.checkout_pr("stan", "performance-tests-cmdstan/cmdstan/stan", params.stan_pr)
                            utils.checkout_pr("math", "performance-tests-cmdstan/cmdstan/stan/lib/stan_math", params.math_pr)

                            sh """
                                cd performance-tests-cmdstan
                                mkdir cmdstan/bin
                                cp ../bin/stanc cmdstan/bin/linux-stanc
                                cd cmdstan; make clean-all; make -j${env.PARALLEL} build; cd ..
                                ./runPerformanceTests.py -j${env.PARALLEL} --runs=0 ../test/integration/good
                                ./runPerformanceTests.py -j${env.PARALLEL} --runs=0 example-models
                                """
                        }

                        xunit([GoogleTest(
                            deleteOutputFiles: false,
                            failIfNotNew: true,
                            pattern: 'performance-tests-cmdstan/performance.xml',
                            skipNoTestFiles: false,
                            stopProcessingIfError: false)
                        ])
                    }
                    post { always { runShell("rm -rf ./*") }}
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
                    agent { label 'linux' }
                    steps {
                        script {
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

                        perfReport modePerformancePerTestCase: true,
                            sourceDataFiles: 'performance-tests-cmdstan/performance.xml',
                            modeThroughput: false,
                            excludeResponseTime: true,
                            errorFailedThreshold: 100,
                            errorUnstableThreshold: 100
                    }
                    post { always { runShell("rm -rf ./*") }}
                }
                stage('Math functions expressions test') {
                    when {
                        beforeAgent true
                        expression {
                            !skipExpressionTests
                        }
                    }
                    agent any
                    steps {

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
                    post { always { deleteDir() } }
                }
            }
        }
        stage("Build and test static release binaries") {
            failFast true
            parallel {
                stage("Build & test Mac OS X binary") {
                    when {
                        beforeAgent true
                        expression {
                            !skipRebuildingBinaries
                        }
                    }
                    agent { label "osx && ocaml" }
                    steps {
                        runShell("""
                            opam switch 4.12.0
                            eval \$(opam env)
                            opam update || true
                            bash -x scripts/install_build_deps.sh
                            dune subst
                            dune build @install
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/mac-stanc"

                        stash name:'mac-exe', includes:'bin/*'
                    }
                    post { always { runShell("rm -rf ./*") }}
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
                            image 'stanorg/stanc3:debian'
                            //Forces image to ignore entrypoint
                            args "-u root --entrypoint=\'\'"
                        }
                    }
                    steps {
                        runShell("""
                            eval \$(opam env)
                            dune subst
                            dune build --profile release src/stancjs
                        """)

                        sh "mkdir -p bin && mv `find _build -name stancjs.bc.js` bin/stanc.js"
                        sh "mv `find _build -name index.html` bin/load_stanc.html"
                        stash name:'js-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
                            image 'stanorg/stanc3:static'
                            //Forces image to ignore entrypoint
                            args "-u 1000 --entrypoint=\'\'"
                        }
                    }
                    steps {
                        runShell("""
                            eval \$(opam env)
                            dune subst
                            dune build @install --profile static
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-stanc"

                        stash name:'linux-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
                            image 'stanorg/stanc3:static'
                            //Forces image to ignore entrypoint
                            args "-u 1000 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                        }
                    }
                    steps {
                        runShell("""
                            eval \$(opam env)
                            dune subst
                        """)
                        sh "sudo apk add docker jq"
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh mips64el"

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-mips64el-stanc"

                        stash name:'linux-mips64el-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
                            image 'stanorg/stanc3:static'
                            //Forces image to ignore entrypoint
                            args "-u 1000 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                        }
                    }
                    steps {
                        runShell("""
                            eval \$(opam env)
                            dune subst
                        """)
                        sh "sudo apk add docker jq"
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh ppc64el"

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-ppc64el-stanc"

                        stash name:'linux-ppc64el-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
                            image 'stanorg/stanc3:static'
                            //Forces image to ignore entrypoint
                            args "-u 1000 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                        }
                    }
                    steps {
                        runShell("""
                            eval \$(opam env)
                            dune subst
                        """)
                        sh "sudo apk add docker jq"
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh s390x"

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-s390x-stanc"

                        stash name:'linux-s390x-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
                            image 'stanorg/stanc3:static'
                            //Forces image to ignore entrypoint
                            label 'linux-ec2'
                            args "-u 1000 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                        }
                    }
                    steps {
                        runShell("""
                            eval \$(opam env)
                            dune subst
                        """)
                        sh "sudo apk add docker jq"
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh arm64"

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-arm64-stanc"

                        stash name:'linux-arm64-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
                            image 'stanorg/stanc3:static'
                            //Forces image to ignore entrypoint
                            label 'linux-ec2'
                            args "-u 1000 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                        }
                    }
                    steps {
                        runShell("""
                            eval \$(opam env)
                            dune subst
                        """)
                        sh "sudo apk add docker jq"
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh armhf"

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-armhf-stanc"

                        stash name:'linux-armhf-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
                            image 'stanorg/stanc3:static'
                            //Forces image to ignore entrypoint
                            label 'linux-ec2'
                            args "-u 1000 --entrypoint=\'\' -v /var/run/docker.sock:/var/run/docker.sock"
                        }
                    }
                    steps {
                        runShell("""
                            eval \$(opam env)
                            dune subst
                        """)
                        sh "sudo apk add docker jq"
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh armel"

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-armel-stanc"

                        stash name:'linux-armel-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
                            image 'stanorg/stanc3:debian-windows'
                            label 'linux-ec2'
                            //Forces image to ignore entrypoint
                            args "-u 1000 --entrypoint=\'\'"
                        }
                    }
                    steps {

                        runShell("""
                            eval \$(opam env)
                            dune subst
                            dune build -x windows
                        """)

                        sh "mkdir -p bin && mv _build/default.windows/src/stanc/stanc.exe bin/windows-stanc"

                        stash name:'windows-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
            agent { label 'linux' }
            environment { GITHUB_TOKEN = credentials('6e7c1e8f-ca2c-4b11-a70e-d934d3f6b681') }
            steps {
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
                    ./ghr_v0.12.1_linux_amd64/ghr -recreate ${tagName()} bin/
                """)
            }
        }
        stage('Upload odoc') {
            when {
                beforeAgent true
                anyOf { buildingTag(); branch 'master' }
            }
            options { skipDefaultCheckout(true) }
            agent {
                docker {
                    image 'stanorg/stanc3:static'
                    label 'gg-linux'
                    //Forces image to ignore entrypoint
                    args "-u 1000 --entrypoint=\'\'"
                }
            }
            steps {
                retry(3) {
                    checkout([$class: 'GitSCM',
                        branches: [],
                        doGenerateSubmoduleConfigurations: false,
                        extensions: [],
                        submoduleCfg: [],
                        userRemoteConfigs: [[url: "https://github.com/stan-dev/stanc3.git", credentialsId: 'a630aebc-6861-4e69-b497-fd7f496ec46b']]]
                    )
                }

                // Install odoc and git-subtree
                runShell("opam install odoc -y")
                runShell("sudo apk update && sudo apk add git-subtree")

                // Checkout gh-pages as a test so we build docs from this branch
                runShell("""
                    git remote set-branches --add origin gh-pages
                    git checkout --track  origin/gh-pages
                    git checkout master
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

                        git config --global user.email "mc.stanislaw@gmail.com"
                        git config --global user.name "Stan Jenkins"

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
          script {utils.mailBuildResults()}
        }
    }
}
