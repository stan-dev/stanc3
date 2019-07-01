@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()

/* Functions that runs a sh command and returns the stdout */
def runShell(String command){
    def output = sh(returnStdout: true, script: "${command}").trim()
    return "${output}"
}

def buildTagImage(String registry, String repository, String dockerfile_path){
    def function = """

        #!/bin/bash

        #Save base location
        base_location=\$(pwd)

        #Copy scripts for Dockerfile
        cp -R scripts "$dockerfile_path/scripts"

        #Build docker image
        cd "$dockerfile_path"       
        sudo docker build -t "$registry/$repository" .
        cd \$base_location

        last_version=\$(curl -u $USERNAME:$PASSWORD https://$registry/v2/$repository/tags/list | jq -S '.tags |= sort' | jq '.tags[-2]' |  tr -d '"')

        #If there isn't any version on the current machine
        #Default to 0.0.0 to be incremented to 0.0.1 when pushing
        if [ -z \$last_version ] || [[ "\$last_version" == "null" ]]; then
            echo "0.0.0" > VERSION
        else
            echo \$last_version > VERSION  
        fi

        echo "last_version: \$last_version"
        echo "VERSION: \$(cat VERSION)"

        #Bump the version
        sudo docker run --rm -v "\$PWD":/app treeder/bump
        version=`cat VERSION`
        echo "version: \$version"
      
        #Old image ID
        old_image_id=\$(sudo docker inspect --format {{.Id}} "$registry/$repository:\$last_version")

        #New image ID
        new_image_id=\$(sudo docker inspect --format {{.Id}} "$registry/$repository:latest")

        if [ "\$old_image_id" == "\$new_image_id" ]; then
            echo "There are no changes in Dockerfile, skipping."
        else
            #Tag the image
            sudo docker tag $registry/$repository:latest $registry/$repository:\$version
                 
            #Push as latest and version
            sudo docker push $registry/$repository:latest
            sudo docker push $registry/$repository:\$version
        fi

    """

    runShell(function)
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
        booleanParam(defaultValue: false, name: 'all_tests',
               description: "Check this box if you want to run all end-to-end tests.")
    }
    stages {
        stage('Kill previous builds') {
            when {
                not { branch 'develop' }
                not { branch 'master' }
                not { branch 'downstream_tests' }
            }
            steps { script { utils.killOldBuilds() } }
        }
        stage ("Build docker images"){
            //when {
            //   branch 'master' 
            //}
            agent { label "docker-registry" }
                    steps {
                        withCredentials([usernamePassword(credentialsId: 'docker-registry-creds', usernameVariable: 'USERNAME', passwordVariable: 'PASSWORD')]) {
                          sh 'sudo docker login registry.mc-stan.org -u="$USERNAME" -p="$PASSWORD"'  

                          //Build and Tag Debian Image
                          buildTagImage("registry.mc-stan.org", "stanc3/debian", "docker/debian")

                          //Build and Tag Debian Image
                          buildTagImage("registry.mc-stan.org", "stanc3/alpine", "docker/static")                   
                        }
                    }
        }
        stage("Build & Test") {
            agent {
                    docker {
                    image 'registry.mc-stan.org/stanc3/debian:latest'
                    registryUrl 'https://registry.mc-stan.org'
                    registryCredentialsId 'docker-registry-creds'
                    args '-u root --entrypoint=\'\''
                }
            }
            steps {
                sh 'printenv'
                runShell("""
                    eval \$(opam env)
                    dune build @install
                """)

                echo runShell("eval \$(opam env); dune runtest --verbose")

                sh "mkdir -p bin && mv _build/default/src/stanc/stanc.exe bin/stanc"
                stash name:'ubuntu-exe', includes:'bin/stanc, notes/working-models.txt'
            }
            post { always { runShell("rm -rf ./*")} }
        }
        stage("Run stat_comp_benchmarks end-to-end") {
            when { not { anyOf { expression { params.all_tests }; buildingTag(); branch 'master' } } }
            agent { label 'linux' }
            steps {
                unstash 'ubuntu-exe'
                sh """
          git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
                   """
                sh """
          cd performance-tests-cmdstan
          echo "CXXFLAGS+=-march=haswell" > cmdstan/make/local
          CXX="${CXX}" ./compare-compilers.sh "stat_comp_benchmarks/ --num-samples=10" "\$(readlink -f ../bin/stanc)"  || true
           cd ..
               """
                junit 'performance-tests-cmdstan/performance.xml'
                archiveArtifacts 'performance-tests-cmdstan/performance.xml'
                perfReport modePerformancePerTestCase: true,
                    sourceDataFiles: 'performance-tests-cmdstan/performance.xml',
                    modeThroughput: false
            }
            post { always { runShell("rm -rf ./*")} }
        }
        // This stage is just gonna try to run all the models we normally
        // do for regression testing
        // and log all the failures. It'll make a big nasty red graph
        // that becomes blue over time as we fix more models :)
        stage("Try to run all models end-to-end") {
            when { anyOf { expression { params.all_tests }; buildingTag(); branch 'master' } }
            agent { label 'linux' }
            steps {
                unstash 'ubuntu-exe'
                sh """
          git clone --recursive --depth 50 https://github.com/stan-dev/performance-tests-cmdstan
                   """
                sh """
          cd performance-tests-cmdstan
          cat known_good_perf_all.tests shotgun_perf_all.tests > all.tests
          cat all.tests
          echo "CXXFLAGS+=-march=haswell" > cmdstan/make/local
          CXX="${CXX}" ./compare-compilers.sh "--tests-file all.tests --num-samples=10" "\$(readlink -f ../bin/stanc)"  || true
               """
                xunit([GoogleTest(
                    deleteOutputFiles: false,
                    failIfNotNew: true,
                    pattern: 'performance-tests-cmdstan/performance.xml',
                    skipNoTestFiles: false,
                    stopProcessingIfError: false)])
                archiveArtifacts 'performance-tests-cmdstan/performance.xml'
                perfReport modePerformancePerTestCase: true,
                    sourceDataFiles: 'performance-tests-cmdstan/performance.xml',
                    modeThroughput: false,
                    excludeResponseTime: true,
                    errorFailedThreshold: 100,
                    errorUnstableThreshold: 100
            }
            post { always {
                runShell("rm -rf ./*")
            } }
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
                        sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/mac-stanc"
                        stash name:'mac-exe', includes:'bin/*'
                    }
                    post {always { runShell("rm -rf ./*")}}
                }
                stage("Build & test a static Linux binary") {
                    agent {
                        docker {
                            image 'registry.mc-stan.org/stanc3/alpine:latest'
                            registryUrl 'https://registry.mc-stan.org'
                            registryCredentialsId 'docker-registry-creds'
                            args '-u opam --entrypoint=\'\''
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
        success {
            script {
                script{
                    println ":)"
                }
            }
        }
        //always {
            //script {utils.mailBuildResults()}
        //}
    }
}
