@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

pipeline {
    agent none
    stages {
        stage('Kill previous builds') {
            when {
                not { branch 'develop' }
                not { branch 'master' }
            }
            steps {
                script {
                    utils.killOldBuilds()
                }
            }
        }
        stage("Build & Test") {
            agent {
                dockerfile {
                    args '-u root --privileged' // TODO: set up a proper user in Dockerfile
                }
            }
            steps {
                stash 'Stanc3'
                sh """
                      eval \$(opam env)
                      dune build @install
                   """
                sh """
                      eval \$(opam env)
                      dune runtest
                   """
            }
        }
        stage("Build & Test static linux binary") {
            agent {
                dockerfile {
                    dir 'docker/static'
                }
            }
            steps {
                unstash 'Stanc3'
                sh """
                      eval \$(opam env)
                      dune build @install
                   """
                sh """
                      eval \$(opam env)
                      dune runtest
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
