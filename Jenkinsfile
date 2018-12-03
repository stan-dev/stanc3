@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

pipeline {
    agent {
        dockerfile {
            args '--privileged' // TODO: set up a proper user in Dockerfile
        }
    }
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
        stage("Build") {
            steps {
                sh """
                      eval \$(opam env)
                      dune build @install --profile static
                   """
            }
        }
        stage("Run all tests") {
            steps {
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
