@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

pipeline {
    agent none
    stages {
        stage("Build & Test") {
            agent {
                dockerfile {
                    filename 'docker/dev-ubuntu/Dockerfile'
                    args "-u root --privileged --entrypoint=''" // TODO: set up a proper user in Dockerfile
                }
            }
            steps {
                sh """
                      eval \$(opam env)
                      dune build @install
                   """
                sh """
                      eval \$(opam env)
                      dune runtest
                   """
                // No idea how the build files from this docker image end
                // up transmitting to the next docker images, so clean here
                // (because the next image can't delete this one's files due
                // to the root user thing)
                sh "git clean -xffd"
            }
        }
        stage("Build & Test static linux binary") {
            agent {
                dockerfile {
                    filename 'docker/static/Dockerfile'
                    // The following is from https://issues.jenkins-ci.org/browse/JENKINS-49385?focusedCommentId=328924&page=com.atlassian.jira.plugin.system.issuetabpanels%3Acomment-tabpanel#comment-328924
                    args "--entrypoint=''"
                }
            }
            steps {
                sh """
                      opam init
                      eval \$(opam env)
                      dune build @install --profile static
                   """
                sh """
                      eval \$(opam env)
                      dune runtest --profile static
                   """
            }
        }
    }
    post {
        always {
            script {utils.mailBuildResults()}
        }
    }

}
