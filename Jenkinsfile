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

                sh "mkdir bin && mv _build/default/src/stanc/stanc.exe bin/stanc"
                stash name:'ubuntu-exe', includes:'bin/stanc, notes/working-models.txt'
            }
            post { always { runShell("rm -rf ./*")} }
        }
        stage("Run stat_comp_benchmarks end-to-end") {
            agent { label 'linux' }
            steps {
                unstash 'ubuntu-exe'
                sh """
          git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
                   """
                sh """
          cd performance-tests-cmdstan
          STANC=\$(readlink -f ../bin/stanc) ./compare-git-hashes.sh stat_comp_benchmarks develop stanc3-dev develop develop
               """
            }
            post { always { runShell("rm -rf ./*")} }
        }
        stage("Run all working models end-to-end on PR merge") {
            agent { label 'linux' }
            steps {
                unstash 'ubuntu-exe'
                sh """
          git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
                   """
                sh """
          cd performance-tests-cmdstan
          STANC=\$(readlink -f ../bin/stanc) ./compare-git-hashes.sh "--tests-file ../notes/working-models.txt" develop stanc3-dev develop develop
               """
            }
            post { always { runShell("rm -rf ./*")} }
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
                    cd scripts && bash -x install_build_deps.sh && cd ..
                    dune subst
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
                    dune subst
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
                    agent { label "windows && WSL" }
                    steps {
                        bat "bash -cl \"cd test/integration\""
                        bat "bash -cl \"find . -type f -name \"*.expected\" -print0 | xargs -0 dos2unix\""
                        bat "bash -cl \"cd ..\""
                        bat "bash -cl \"eval \$(opam env) make clean; dune subst; dune build -x windows; dune runtest --verbose\""
                        bat """bash -cl "rm -rf bin/*; mkdir -p bin; mv _build/default.windows/src/stanc/stanc.exe bin/windows-stanc" """
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
