@Library('StanUtils')
import org.stan.Utils

def utils = new org.stan.Utils()
def docker_user = ""

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
        stage("Get linux user"){
            agent { label 'docker'}
            steps{
                    script{
                        docker_user = runShell("getent passwd \"1004\" | cut -d: -f1")

                        if(docker_user == "opam"){
                            docker_user = "opam"
                        }
                        else{
                            docker_user = runShell("echo \$(id -u)")
                        }
                }    
            }     
        }
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
                        dockerfile {
                            filename 'docker/static/Dockerfile'
                            //Forces image to ignore entrypoint
                            args "-u ${docker_user}"
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
    //post {
    //   always {
            //script {utils.mailBuildResults()}
    //    }
    //}
}
