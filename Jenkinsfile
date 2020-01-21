@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

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
        booleanParam(name:"compile_all", defaultValue: false,
                     description:"Try compiling all models in test/integration/good")
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
        stage("Build") {
            agent {
                dockerfile {
                    filename 'docker/debian/Dockerfile'
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
        stage("Test") {
            parallel {
                stage("Dune tests") {
                    agent {
                        dockerfile {
                            filename 'docker/debian/Dockerfile'
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
                stage("Try to compile all good integration models") {
                    when { expression { params.compile_all } }
                    agent { label 'linux' }
                    steps {
                        unstash 'ubuntu-exe'
                        sh """
                            git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
                        """

                        writeFile(file:"performance-tests-cmdstan/cmdstan/make/local",
                                  text:"O=0\nCXXFLAGS+=-o/dev/null -S -Wno-unused-command-line-argument")
                        sh """
                            cd performance-tests-cmdstan
                            cd cmdstan; make -j${env.PARALLEL} build; cd ..
                            cp ../bin/stanc cmdstan/bin/stanc
                            git clone --depth 1 https://github.com/stan-dev/stanc3
                            CXX="${CXX}" ./runPerformanceTests.py --runs=0 stanc3/test/integration/good || true
                        """

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
                stage("Run all models end-to-end") {
                    agent { label 'linux' }
                    steps {
                        unstash 'ubuntu-exe'
                        sh """
                            git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
                        """
                        sh """
                            cd performance-tests-cmdstan
                            git show HEAD --stat
                            echo "example-models/regression_tests/mother.stan" > all.tests
                            cat known_good_perf_all.tests >> all.tests
                            echo "" >> all.tests
                            cat shotgun_perf_all.tests >> all.tests
                            cat all.tests
                            echo "CXXFLAGS+=-march=core2" > cmdstan/make/local
                            cd cmdstan; git show HEAD --stat; STANC2=true make -j4 build; cd ..
                            CXX="${CXX}" ./compare-compilers.sh "--tests-file all.tests --num-samples=10" "\$(readlink -f ../bin/stanc)"
                        """

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
                stage("TFP tests") {
                    agent {
                        docker {
                            image 'tensorflow/tensorflow@sha256:4be8a8bf5e249fce61d8bedc5fd733445962c34bf6ad51a16f9009f125195ba9'
                            args '-u root'
                        }
                    }
                    steps {
                        sh "pip3 install tfp-nightly==0.9.0.dev20191216"
                        sh "python3 test/integration/tfp/tests.py"
                    }
                    post { always { runShell("rm -rf ./*") }}
                }
            }
        }
        stage("Build and test static release binaries") {
            when { anyOf { buildingTag(); branch 'master' } }
            failFast true
            parallel {
                stage("Build & test Mac OS X binary") {
                    agent { label "osx && ocaml" }
                    steps {
                        runShell("""
                            eval \$(opam env)
                            opam update || true
                            bash -x scripts/install_build_deps.sh
                            dune subst
                            dune build @install
                        """)

                        echo runShell("""
                            eval \$(opam env)
                            time dune runtest --verbose
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/mac-stanc"
                        sh "mv _build/default/src/stan2tfp/stan2tfp.exe bin/mac-stan2tfp"

                        stash name:'mac-exe', includes:'bin/*'
                    }
                    post { always { runShell("rm -rf ./*") }}
                }
                stage("Build stanc.js") {
                    agent {
                        dockerfile {
                            filename 'docker/debian/Dockerfile'
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
                    agent {
                        dockerfile {
                            filename 'docker/static/Dockerfile'
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

                        echo runShell("""
                            eval \$(opam env)
                            time dune runtest --profile static --verbose
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-stanc"
                        sh "mv `find _build -name stan2tfp.exe` bin/linux-stan2tfp"

                        stash name:'linux-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
                }
                stage("Build & test static Windows binary") {
                    agent { label "WSL" }
                    steps {
                        bat "bash -cl \"cd test/integration\""
                        bat "bash -cl \"find . -type f -name \"*.expected\" -print0 | xargs -0 dos2unix\""
                        bat "bash -cl \"cd ..\""
                        bat "bash -cl \"eval \$(opam env) make clean; dune subst; dune build -x windows; dune runtest --verbose\""
                        bat """bash -cl "rm -rf bin/*; mkdir -p bin; mv _build/default.windows/src/stanc/stanc.exe bin/windows-stanc" """
                        bat "bash -cl \"mv _build/default.windows/src/stan2tfp/stan2tfp.exe bin/windows-stan2tfp\""
                        stash name:'windows-exe', includes:'bin/*'
                    }
                }
            }
        }
        stage("Release tag and publish binaries") {
            when { anyOf { buildingTag(); branch 'master' } }
            agent { label 'linux' }
            environment { GITHUB_TOKEN = credentials('6e7c1e8f-ca2c-4b11-a70e-d934d3f6b681') }
            steps {
                unstash 'windows-exe'
                unstash 'linux-exe'
                unstash 'mac-exe'
                unstash 'js-exe'
                runShell("""
                    wget https://github.com/tcnksm/ghr/releases/download/v0.12.1/ghr_v0.12.1_linux_amd64.tar.gz
                    tar -zxvpf ghr_v0.12.1_linux_amd64.tar.gz
                    ./ghr_v0.12.1_linux_amd64/ghr -recreate ${tagName()} bin/
                """)
            }
        }
    }
    post {
       always {
          script {utils.mailBuildResults()}
        }
    }
}
