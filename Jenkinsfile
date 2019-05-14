@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

/* Functions that runs a sh command and returns the stdout */
def runShell(String command){
    def output = sh (returnStdout: true, script: "${command}").trim()
    return "${output}"
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
                /* runs 'dune build @install'*/
                runShell("""
                    eval \$(opam env)
                    dune build @install
                """)

                /*Logs the start time of tests*/
                runShell("echo \$(date +'%s') > time.log")

                /* runs 'dune runtest' */
                echo runShell("""
                    eval \$(opam env)
                    dune runtest --verbose
                """)

                /*Echoes time elapsed for tests*/
                echo runShell("echo \"It took \$((\$(date +'%s') - \$(cat time.log))) seconds to run the tests\"")

                //Cleans the workspace
                runShell("rm -rf ./*")
            }
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
                    mkdir -p _build/default
                    git clone --recursive https://github.com/stan-dev/cmdstan _build/default/test/integration/good/code-gen/cmdstan
                    cd _build/default/test/integration/good/code-gen/cmdstan && make -j${env.PARALLEL} build && cd ../../../../../../..
                """
                sh """
                    eval \$(opam env)
                    ls _build/default/test/integration/good/code-gen/cmdstan
                    ls _build/default/test/integration/good/code-gen/cmdstan/stan/lib/stan_math
                    cmdstan=cmdstan dune runtest test/integration/good/code-gen
                """
            }
        }
        stage("Build & Test windows binary") {
            agent { label 'windows' }
            steps {
                bat "bash -cl \"cd test/integration\""
                bat "bash -cl \"find . -type f -name \"*.expected\" -print0 | xargs -0 dos2unix\""
                bat "bash -cl \"cd ..\""
                bat "bash -cl \"eval \$(opam env) make clean; dune build -x windows; dune runtest\""
            }
        }
        stage("Build & Test static linux binary") {
            agent {
                dockerfile {
                    filename 'docker/static/Dockerfile'
                    //Forces image to ignore entrypoint
                    args "-u root --entrypoint=\'\'"
                }
            }
            steps {

                /* runs 'dune build @install' command and then outputs the stdout*/
                runShell("""
                    eval \$(opam env)
                    dune build @install --profile static
                """)

                /*Logs the start time of tests*/
                runShell("echo \$(date +'%s') > time.log")

                /* runs 'dune runtest' command and then outputs the stdout*/
                echo runShell("""
                    eval \$(opam env)
                    dune runtest --profile static --verbose
                """)

                /*Echoes time elapsed for tests*/
                echo runShell("echo \"It took \$((\$(date +'%s') - \$(cat time.log))) seconds to run the tests\"")

                //Cleans the workspace
                runShell("rm -rf ./*")

            }
        }
    }
    post {
        always {
            script {utils.mailBuildResults()}
        }
    }
}
