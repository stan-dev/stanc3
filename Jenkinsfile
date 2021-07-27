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
                        sh "sudo apk add docker"
                        sh "sudo bash -x scripts/build_multiarch_stanc3.sh armhf"
                        sh "sudo chown -R opam: _build"
                        sh "sudo chown -R opam: src"
                        sh "sudo chown -R opam: test"
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

            }

        }
        stage("Release tag and publish binaries") {
            when { anyOf { buildingTag(); branch 'master' } }
            agent { label 'linux' }
            environment { GITHUB_TOKEN = credentials('6e7c1e8f-ca2c-4b11-a70e-d934d3f6b681') }
            steps {
                //unstash 'windows-exe'
                unstash 'linux-exe'
                unstash 'linux-armhf-exe'
                //unstash 'mac-exe'
                //unstash 'linux-arm-exe'
                //unstash 'js-exe'
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
