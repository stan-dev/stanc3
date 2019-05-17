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
                   git clone --recursive https://github.com/stan-dev/cmdstan
                   cd cmdstan && make -j${env.PARALLEL} build && cd ..
               """
                sh """
                   eval \$(opam env)
                   dune --version
                   ls cmdstan
                   ls "`pwd`/cmdstan"
                   CMDSTAN="`pwd`/cmdstan" dune runtest test/integration/good/code-gen
               """
            }
            post { always { runShell("rm -rf ./*")} }
        }
        stage("Build & Test windows binary") {
            agent { label 'windows' }
            steps {
                bat "bash -cl \"cd test/integration\""
                bat "bash -cl \"find . -type f -name \"*.expected\" -print0 | xargs -0 dos2unix\""
                bat "bash -cl \"cd ..\""
                bat "bash -cl \"eval \$(opam env) make clean; dune build -x windows; dune runtest\""

                archiveArtifacts '_build/**/stanc.exe', onlyIfSuccessful: true
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

                archiveArtifacts '_build/**/stanc.exe', onlyIfSuccessful: true
            }
            post {always { runShell("rm -rf ./*")}}
        }
    }
    post {
        always {
            script {utils.mailBuildResults()}
        }
    }
}
