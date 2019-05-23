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
    stages {
        stage("Build & Test") {
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

                runShell("echo \$(date +'%s') > time.log")

                echo runShell("""
                    eval \$(opam env)
                    dune runtest --verbose
                """)

                echo runShell("echo \"It took \$((\$(date +'%s') - \$(cat time.log))) seconds to run the tests\"")
            }
            post { always { runShell("rm -rf ./*")} }
        }
        stage("Run end-to-end tests") {
            agent {
                dockerfile {
                    filename 'docker/debian/Dockerfile'
                    //Forces image to ignore entrypoint
                    args "-u root --entrypoint=\'\'"
                }
            }
            steps {
                sh """
                   git clone -j${env.PARALLEL} --shallow-submodules --recursive --depth 1 --branch develop https://github.com/stan-dev/cmdstan
                   cd cmdstan && make -j${env.PARALLEL} build && cd ..
               """
                sh """
                   eval \$(opam env)
                   CMDSTAN="`pwd`/cmdstan" dune runtest test/integration/good/code-gen
               """
            }
            post { always { runShell("rm -rf ./*")} }
        }
        stage("Build & test Mac OS X binary") {
            when { anyOf { buildingTag(); branch 'master' } }
            agent { label 'osx' }
            steps {
                runShell("""
                    eval \$(opam env)
                    cd scripts && bash -x install_build_deps.sh && cd ..
                    dune build @install
                """)

                echo runShell("""
                    eval \$(opam env)
                    time dune runtest --verbose
                """)

                sh "mkdir bin && mv `find _build -name stanc.exe` bin/mac-stanc"
                stash name:'mac-exe', includes:'bin/*'
            }
            post {always { runShell("rm -rf ./*")}}
        }
        stage("Build & test a static Linux binary") {
            when { anyOf { buildingTag(); branch 'master' } }
            agent {
                dockerfile {
                    filename 'docker/static/Dockerfile'
                    //Forces image to ignore entrypoint
                    args "-u root --entrypoint=\'\'"
                }
            }
            steps {
                runShell("""
                    eval \$(opam env)
                    dune build @install --profile static
                """)

                echo runShell("""
                    eval \$(opam env)
                    time dune runtest --profile static --verbose
                """)

                sh "mkdir bin && mv `find _build -name stanc.exe` bin/linux-stanc"
                stash name:'linux-exe', includes:'bin/*'
            }
            post {always { runShell("rm -rf ./*")}}
        }
        stage("Build & test static Windows binary") {
            when { anyOf { buildingTag(); branch 'master' } }
            agent { label 'windows' }
            steps {
                bat "bash -cl \"cd test/integration\""
                bat "bash -cl \"find . -type f -name \"*.expected\" -print0 | xargs -0 dos2unix\""
                bat "bash -cl \"cd ..\""
                bat "bash -cl \"eval \$(opam env) make clean; dune build -x windows; dune runtest --verbose -x windows\""
                bat """bash -cl "rm -rf bin/*; mkdir -p bin; mv _build/default.windows/src/stanc/stanc.exe bin/windows-stanc" """
                stash name:'windows-exe', includes:'bin/*'
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
                runShell("""wget https://github.com/tcnksm/ghr/releases/download/v0.12.1/ghr_v0.12.1_linux_amd64.tar.gz
                            tar -zxvpf ghr_v0.12.1_linux_amd64.tar.gz
                            ./ghr_v0.12.1_linux_amd64/ghr -recreate ${tagName()} bin/ """)
            }
        }
    }
    post {
        always {
            script {utils.mailBuildResults()}
        }
    }
}
