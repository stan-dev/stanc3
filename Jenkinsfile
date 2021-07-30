@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()
def skipExpressionTests = false
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
        booleanParam(name:"compile_all", defaultValue: false, description:"Try compiling all models in test/integration/good")
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

                    def stanMathSigs = ['test/integration/signatures/stan_math_sigs.expected'].join(" ")
                    skipExpressionTests = utils.verifyChanges(stanMathSigs)
                    
                    if (buildingTag()) {
                        buildingAgentARM = "arm-ec2"
                    }
                }
            }
        }

        stage("Build and test static release binaries") {
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

                stage("Build & test a static Linux mips64el binary") {
                    agent {
                        dockerfile {
                            filename 'docker/static/Dockerfile'
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
                        echo runShell("""
                            eval \$(opam env)
                            time dune runtest --profile static --verbose
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-mips64el-stanc"
                        sh "mv `find _build -name stan2tfp.exe` bin/linux-mips64el-stan2tfp"

                        stash name:'linux-mips64el-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
                }

                stage("Build & test a static Linux ppc64el binary") {
                    agent {
                        dockerfile {
                            filename 'docker/static/Dockerfile'
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
                        echo runShell("""
                            eval \$(opam env)
                            time dune runtest --profile static --verbose
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-ppc64el-stanc"
                        sh "mv `find _build -name stan2tfp.exe` bin/linux-ppc64el-stan2tfp"

                        stash name:'linux-ppc64el-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
                }

                stage("Build & test a static Linux s390x binary") {
                    agent {
                        dockerfile {
                            filename 'docker/static/Dockerfile'
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
                        echo runShell("""
                            eval \$(opam env)
                            time dune runtest --profile static --verbose
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-s390x-stanc"
                        sh "mv `find _build -name stan2tfp.exe` bin/linux-s390x-stan2tfp"

                        stash name:'linux-s390x-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
                }

                stage("Build & test a static Linux arm64 binary") {
                    agent {
                        dockerfile {
                            filename 'docker/static/Dockerfile'
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
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh arm64"
                        echo runShell("""
                            eval \$(opam env)
                            time dune runtest --profile static --verbose
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-arm64-stanc"
                        sh "mv `find _build -name stan2tfp.exe` bin/linux-arm64-stan2tfp"

                        stash name:'linux-arm64-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
                }

                stage("Build & test a static Linux armhf binary") {
                    agent {
                        dockerfile {
                            filename 'docker/static/Dockerfile'
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
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh armhf"
                        echo runShell("""
                            eval \$(opam env)
                            time dune runtest --profile static --verbose
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-armhf-stanc"
                        sh "mv `find _build -name stan2tfp.exe` bin/linux-armhf-stan2tfp"

                        stash name:'linux-armhf-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
                }

                stage("Build & test a static Linux armel binary") {
                    agent {
                        dockerfile {
                            filename 'docker/static/Dockerfile'
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
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh armel"
                        echo runShell("""
                            eval \$(opam env)
                            time dune runtest --profile static --verbose
                        """)

                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/linux-armel-stanc"
                        sh "mv `find _build -name stan2tfp.exe` bin/linux-armel-stan2tfp"

                        stash name:'linux-armel-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
                }

                // Cross compiling for windows on debian
                stage("Build & test static Windows binary") {
                    agent {
                        dockerfile {
                            filename 'docker/debian-windows/Dockerfile'
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

                        echo runShell("""
                            eval \$(opam env)
                            time dune runtest --verbose
                        """)

                        sh "mkdir -p bin && mv _build/default.windows/src/stanc/stanc.exe bin/windows-stanc"
                        sh "mv _build/default.windows/src/stan2tfp/stan2tfp.exe bin/windows-stan2tfp"

                        stash name:'windows-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
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
    }
    post {
       always {
          script {utils.mailBuildResults()}
        }
    }
}
