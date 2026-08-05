properties([
  disableConcurrentBuilds(abortPrevious: true),
  buildDiscarder(logRotator(numToKeepStr: '20', daysToKeepStr: '30')),
  parameters([
    booleanParam(name:"skip_end_to_end", defaultValue: false, description:"Skip end-to-end tests "),
    booleanParam(name:"run_slow_perf_tests", defaultValue: false, description:"Run additional 'slow' performance tests"),
    booleanParam(name:"skip_compile_O1", defaultValue: false, description:"Skip compile tests that run with STANCFLAGS = --O1"),
    booleanParam(name:"skip_compile", defaultValue: false, description:"Skip compile tests"),
    booleanParam(name:"skip_math_func_expr", defaultValue: false, description:"Skip math functions expressions test"),
    booleanParam(name:"skip_ocaml_tests", defaultValue: false, description:"Skip ocaml tests"),
    booleanParam(name:"build_multiarch", defaultValue: false, description:"Build multiarch images even when not on 'master'"),
    booleanParam(name:"test_binaries", defaultValue: false, description:"Build and archive all binaries"),
    booleanParam(name:"run_all", defaultValue: false, description:"Pretend all files changed"),

    string(defaultValue: 'develop', name: 'cmdstan_pr',
           description: "CmdStan PR to test against. Will check out this PR in the downstream Stan repo."),
    string(defaultValue: 'develop', name: 'stan_pr',
           description: "Stan PR to test against. Will check out this PR in the downstream Stan repo."),
    string(defaultValue: 'develop', name: 'math_pr',
           description: "Math PR to test against. Will check out this PR in the downstream Math repo."),
    string(defaultValue: '', name: 'stanc_flags',
           description: "Pass STANCFLAGS to make/local, default none")
  ])
])

def runExpressionTests = false
def runRemainingStages = false
def runCompileTests = false
def runRebuildingBinaries = false
def runCompileTestsAtO1 = false

catchError {
  withEnv([
    'CXX=clang++-6.0',
    'MACOS_SWITCH=stanc3-ocaml5.5-nobase',
    'GIT_AUTHOR_NAME=Stan Jenkins',
    'GIT_AUTHOR_EMAIL=mc.stanislaw@gmail.com',
    'GIT_COMMITTER_NAME=Stan Jenkins',
    'GIT_COMMITTER_EMAIL=mc.stanislaw@gmail.com'
  ]) {
    buildImage(tag: 'ci',
      context: 'scripts',
      dockerfile: 'docker/ci/Dockerfile',
      prep: {
        def srcChanged = params.run_all || filesChanged("src/")
        runExpressionTests = params.run_all || filesChanged('test/integration/signatures/stan_math_signatures.t')
        runCompileTests = params.run_all || filesChanged('test/integration/good/')
        runRemainingStages = runCompileTests || srcChanged || filesChanged('test/stancjs/')
        runCompileTestsAtO1 = params.run_all || runCompileTests && filesChanged('test/integration/good/compiler-optimizations')
        runRebuildingBinaries = params.test_binaries || srcChanged || filesChanged('Jenkinsfile')
        return null
      })

    if (runRebuildingBinaries || runRemainingStages) {
      def buildBinary = { arch ->
        runPod(tag: arch ?: 'static', emulation: !!arch, cpus: 2) {
          stage("Build ${arch ?: 'Linux'} binary") {
            sh '''
                eval $(opam env)
                dune subst
                dune build --profile static -j$PARALLEL
            '''
            def label = arch ? "-$arch" : ""
            def bin = "bin/linux$label-stanc"
            sh "mkdir -p bin && mv `find _build -name stanc.exe` $bin"
            sh "chmod +w $bin && strip $bin"
            sh "./$bin --version"
            stash name:"linux${label}-exe", includes:bin
          }
        }
      }

      buildImage(tag: 'static',
        context: 'scripts',
        dockerfile: 'docker/static-builder/Dockerfile',
        buildArgs: "--platform=linux/amd64")
      buildBinary(null)

      if (runRemainingStages) {
        runPod(tag: 'ci', cpus: 4) {
          stage("Code formatting") {
            sh """
                eval \$(opam env)
                make format  ||
                (
                    set +x &&
                    echo "The source code was not formatted. Please run 'make format; dune promote' and push the changes." &&
                    echo "Please consider installing the pre-commit git hook for formatting with the above command." &&
                    echo "Our hook can be installed with bash ./scripts/hooks/install_hooks.sh" &&
                    exit 1;
                )
            """
          }
          if (!params.skip_ocaml_tests) {
            stage("Dune tests") {
              sh '''
                  eval $(opam env)
                  make testcoverage
              '''

              withCredentials([string(credentialsId: 'stanc3-codecov-token', variable: 'CODECOV_TOKEN')]) {
                  sh "codecov -v -C \$(git rev-parse HEAD)"
              }
            }
            stage("stancjs tests") {
              sh '''
                  node --version
                  eval $(opam env)
                  dune build @runjstest -j$PARALLEL
              '''
            }
          }
        }
      }

      stage("CmdStan & Math tests") {
        def runPerformanceTests = { String testsPath, String stancFlags ->
          dir('cmdstan') {
            def local = """\
              CXX=$CXX
              O=0
              CXXFLAGS+=-Wall
            """
            if (stancFlags) local += """
              STANCFLAGS=$stancFlags
            """
            writeFile(file: "make/local", text: local.stripIndent())
            sh '''
                make clean-all
                make -j$PARALLEL build
            '''
          }

          def args = params.run_slow_perf_tests ? "--no-ignore-models" : ""
          sh """
              ./runPerformanceTests.py -j\$PARALLEL --runs=0 $args ${testsPath}
          """
        }
        def endToEnd = { optimize ->
          sh '''
              git show HEAD --stat
              echo "example-models/regression_tests/mother.stan" > all.tests
          '''
          if (optimize) sh '''
              cat optimizer.tests >> all.tests
              echo "" >> all.tests
            '''
          sh '''
              cat known_good_perf_all.tests >> all.tests
              echo "" >> all.tests
              cat shotgun_perf_all.tests >> all.tests
              cat all.tests
              echo "CXXFLAGS+=-march=core2" > cmdstan/make/local
              echo "PRECOMPILED_HEADERS=false" >> cmdstan/make/local
              cd cmdstan; make clean-all; git show HEAD --stat; cd ..
          '''
          sh optimize ? '''
              CXX="$CXX" ./compare-optimizer.sh "--tests-file all.tests --num-samples=10 -j$PARALLEL" "--O1" "$(readlink -f ../bin/linux-stanc)"
          ''' : '''
              CXX="$CXX" ./compare-compilers.sh "--tests-file all.tests --num-samples=10 -j$PARALLEL" "$(readlink -f ../bin/linux-stanc)"
          '''
          archiveArtifacts 'performance.xml'
        }
        parallel compile: {
          if (runCompileTests && !params.skip_compile) {
            runPod(image: 'stanorg/ci:gpu', cpus: 16, memory: "64Gi") {
              unstash 'linux-exe'
              dir('performance-tests-cmdstan') {
                checkout scmGit(
                  userRemoteConfigs: [[url: 'https://github.com/stan-dev/performance-tests-cmdstan']],
                  branches: [[name: 'refs/heads/master']],
                  extensions: [
                    cloneOption(noTags: true, shallow: true, depth: 50),
                    submodule(shallow: true, depth: 8, recursiveSubmodules: true)])
                checkoutPR("cmdstan", params.cmdstan_pr)
                checkoutPR("cmdstan/stan", params.stan_pr)
                checkoutPR("cmdstan/stan/lib/stan_math", params.math_pr)
                sh """
                    mkdir cmdstan/bin
                    cp ../bin/linux-stanc cmdstan/bin/linux-stanc
                """

                stage("Compile tests - good") {
                  runPerformanceTests("../test/integration/good", params.stanc_flags)
                }
                stage("Compile tests - example-models") {
                  runPerformanceTests("example-models", params.stanc_flags)
                }

                if (!params.skip_end_to_end) {
                  stage("Model end-to-end tests") {
                    endToEnd(false)
                  }
                }
              }
            }
          }
        }, compileAtO1: {
          if (runCompileTestsAtO1 && !params.skip_compile_O1) {
            runPod(image: 'stanorg/ci:gpu', cpus: 16, memory: "64Gi") {
              unstash 'linux-exe'
              dir('performance-tests-cmdstan') {
                checkout scmGit(
                  userRemoteConfigs: [[url: 'https://github.com/stan-dev/performance-tests-cmdstan']],
                  branches: [[name: 'refs/heads/master']],
                  extensions: [
                    cloneOption(noTags: true, shallow: true, depth: 50),
                    submodule(shallow: true, depth: 8, recursiveSubmodules: true)])
                checkoutPR("cmdstan", params.cmdstan_pr)
                checkoutPR("cmdstan/stan", params.stan_pr)
                checkoutPR("cmdstan/stan/lib/stan_math", params.math_pr)
                sh """
                    mkdir cmdstan/bin
                    cp ../bin/linux-stanc cmdstan/bin/linux-stanc
                """


                stage("Compile tests - good at O=1") {
                  runPerformanceTests("../test/integration/good", "--O1")
                }
                stage("Compile tests - example-models at O=1") {
                  runPerformanceTests("example-models", "--O1")
                }

                if (!params.skip_end_to_end) {
                  stage("Model end-to-end tests at O=1") {
                    endToEnd(true)
                  }
                }
              }
            }
          }
        }, math: {
          if (runRemainingStages && runExpressionTests && !params.skip_math_func_expr) {
            runPod(image: 'stanorg/ci:gpu', cpus: 8, memory: '64Gi') {
              stage('Math functions expressions test') {
                unstash 'linux-exe'
                dir('math') {
                  checkout scmGit(
                    userRemoteConfigs: [[url: 'https://github.com/stan-dev/math']],
                    branches: [[name: 'refs/heads/master']],
                    extensions: [
                      cloneOption(noTags: true, shallow: true, depth: 50),
                      submodule(shallow: true, depth: 8, recursiveSubmodules: true)])
                  checkoutPR(".", params.math_pr)
                  sh """
                      cp ../bin/linux-stanc test/expressions/stanc
                      echo O=0 >> make/local
                      echo "CXX=${env.CXX} -Werror " >> make/local
                  """
                  withEnv(['PATH+TBB=./lib/tbb']) {
                    catchError(buildResult: 'UNSTABLE', stageResult: 'UNSTABLE') {
                      sh './runTests.py -j$PARALLEL test/expressions'
                    }
                    junit 'test/**/*.xml'
                  }
                }
              }
            }
          }
        }
      }

      if (runRebuildingBinaries) {
        stage('Build binaries') {
          parallel macos: {
            stage("Build MacOS x86 binary") {
              node('macos && intel') {
                dir("osx-x86") {
                  checkout scm
                  withEnv([
                    'SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX10.11.sdk',
                     'MACOSX_DEPLOYMENT_TARGET=10.11'
                  ]) {
                   sh '''
                       export PATH=/Users/jenkins/brew/bin:$PATH
                       bash -x scripts/install_ocaml.sh "$MACOS_SWITCH"
                       opam switch list
                   '''
                   }
                 // dune can't be built with the older SDK
                 sh '''
                     export PATH=/Users/jenkins/brew/bin:$PATH
                     eval $(opam env --switch="$MACOS_SWITCH" --set-switch)
                     opam update
                     opam install -y dune dune-build-info
                 '''
                  withEnv([
                    'SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX10.11.sdk',
                     'MACOSX_DEPLOYMENT_TARGET=10.11'
                  ]) {
                   sh '''
                       export PATH=/Users/jenkins/brew/bin:$PATH
                       eval $(opam env --switch="$MACOS_SWITCH" --set-switch)
                       opam update
                       bash -x scripts/install_build_deps.sh
                       dune subst
                       dune build --profile=release
                   '''
                   }
                  sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/mac-x86-stanc"
                  stash name:'mac-x86-exe', includes:'bin/mac-x86-stanc'
                }
              }
            }
            stage("Build MacOS arm64 binary") {
              node('macos && m1') {
                dir("osx-arm64") {
                  checkout scm
                  withEnv([
                    'SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX11.0.sdk',
                    'MACOSX_DEPLOYMENT_TARGET=11.0'
                  ]) {
                    sh '''
                        export PATH=$BREW/bin:$PATH
                        bash -x scripts/install_ocaml.sh "$MACOS_SWITCH"
                        eval $(opam env --switch="$MACOS_SWITCH" --set-switch)
                        opam switch list
                        opam update -y || true
                        bash -x scripts/install_build_deps.sh
                        dune subst
                        dune build --profile=release
                    '''
                  }
                  sh "mkdir -p bin && mv `find _build -name stanc.exe` bin/mac-arm64-stanc"
                  stash name:'mac-arm64-exe', includes:'bin/mac-arm64-stanc'
                }
              }
            }
            stage('Build MacOS fat binary') {
              node('macos && m1') {
                dir("osx-universal") {
                  deleteDir()
                  unstash 'mac-arm64-exe'
                  unstash 'mac-x86-exe'
                  def bin = "bin/mac-stanc"
                  withEnv([
                    'SDKROOT=/Library/Developer/CommandLineTools/SDKs/MacOSX11.0.sdk',
                    'MACOSX_DEPLOYMENT_TARGET=11.0'
                  ]) {
                    sh "lipo -create bin/mac-x86-stanc bin/mac-arm64-stanc -output $bin"
                  }
                  sh "lipo -archs $bin"
                  sh "chmod +w $bin && strip $bin"
                  sh "./$bin --version"
                  stash name:'mac-exe', includes:bin
                }
              }
            }
          }, linux: {
            runPod(tag: 'ci', cpus: 2) {
              stage("Build stanc.js") {
                sh '''
                    eval $(opam env)
                    dune subst
                    dune build --profile release src/stancjs -j$PARALLEL
                    mkdir -p bin && mv `find _build -name stancjs.bc.js` bin/stanc.js
                    dune build --force --profile=dev src/stancjs -j$PARALLEL
                    mv `find _build -name stancjs.bc.js` bin/stanc-pretty.js
                '''
                stash name:'js-exe', includes:'bin/stanc.js,bin/stanc-pretty.js'
              }
              stage("Generate shell support files") {
                sh '''
                    eval $(opam env)
                    dune build -j$PARALLEL
                    cmdliner install tool-support --standalone-completion "./_build/default/src/stanc/stanc.exe:stanc" shell-support/
                    cd shell-support
                    zip -r ../shell-support-files.zip *
                '''
                stash name: 'shell-support', includes:'shell-support-files.zip'
              }
              // Cross compiling for windows on debian
              stage("Build Windows binary") {
                sh '''
                    eval $(opam env)
                    dune build -x windows --profile=release -j$PARALLEL
                    mkdir -p bin && mv _build/default.windows/src/stanc/stanc.exe bin/windows-stanc
                    chmod +w bin/windows-stanc && strip bin/windows-stanc
                '''
                stash name:'windows-exe', includes:'bin/windows-stanc'
              }
            }
          }, multiarch: {
            if (params.test_binaries || params.build_multiarch || env.TAG_NAME || env.BRANCH_NAME == "master") {
              stage('Build other architectures') {
                def archs = [
                  'arm64':   'arm64',
                  'ppc64el': 'ppc64le',
                  's390x':   's390x',
                  'armhf':   'arm/v7',
                  'armel':   'arm/v6'
                ]
                buildImages(archs.collect { arch, platform ->
                  [ tag: arch,
                   context: 'scripts',
                   dockerfile: 'docker/static-builder/Dockerfile',
                   buildArgs: "--platform=linux/$platform"
                  ]
                }, checkout: true)
                parallel(archs.collectEntries{arch, platform -> [arch, { buildBinary(arch) }]})
              }
            }
          }
        }
      }

      if (params.test_binaries || runRemainingStages && runRebuildingBinaries && (env.TAG_NAME || env.BRANCH_NAME == "master")) {
        runPod(tag: 'ci') {
          stage("Release tag and publish binaries") {
            unstash 'windows-exe'
            unstash 'linux-exe'
            unstash 'mac-exe'
            unstash 'mac-arm64-exe'
            unstash 'mac-x86-exe'
            unstash 'linux-ppc64el-exe'
            unstash 'linux-s390x-exe'
            unstash 'linux-arm64-exe'
            unstash 'linux-armhf-exe'
            unstash 'linux-armel-exe'
            unstash 'js-exe'
            dir("bin") {
              unstash 'shell-support'
            }
            if (params.test_binaries) {
              archiveArtifacts 'bin/*'
            } else {
              def tagName = env.TAG_NAME ?: env.BRANCH_NAME == 'master' ? 'nightly' : 'unknown'
              retry(3) {
                withCredentials([usernamePassword(usernameVariable: 'GITHUB_USER', passwordVariable: 'GITHUB_TOKEN', credentialsId: 'stan-github')]) {
                  sh """
                      gh release delete $tagName ${tagName == 'nightly' ? '--cleanup-tag' : ''} -y || true
                      gh release create $tagName --latest --target master --notes "\$(git log --pretty=format:'nightly: %h %s' -n 1)" ./bin/*
                  """
                }

                // Update stanc.js in StanHeaders
                dir('rstan') {
                  def branch = "develop"
                  def scm = scmGit(
                    branches: [[name: "refs/heads/$branch"]],
                    userRemoteConfigs: [[credentialsId: 'stan-github', url: 'https://github.com/stan-dev/rstan.git']])
                  checkout(scm: scm, changelog: false, poll: false)
                  sh """
                    rm StanHeaders/inst/stanc.js
                    cp ../bin/stanc.js StanHeaders/inst/stanc.js
                    git add StanHeaders/inst/stanc.js
                    git commit --allow-empty -m "Update stanc.js to ${tagName}" --author="\$GIT_AUTHOR_NAME <\$GIT_AUTHOR_EMAIL>"
                  """
                  gitPush(gitScm: scm, targetBranch: branch, targetRepo: 'origin')
                }
              }
            }
          }
        }
      }
    }

    if (env.BRANCH_NAME == "master") {
      runPod(tag: 'ci') {
        stage('Upload odoc') {
          def branch = 'gh-pages'
          sh '''
              eval $(opam env)
              dune build @doc
          '''
          dir('gh-pages') {
            def scm = scmGit(
              branches: [[name: "refs/heads/$branch"]],
              userRemoteConfigs: [[credentialsId: 'stan-github', url: 'https://github.com/stan-dev/stanc3.git']])
            checkout(scm: scm, changelog: false, poll: false)
            // Commit and push
            sh '''
              rm -rf *
              mv ../_build/default/_doc/_html/* .
              git add .
              git commit --amend -m "auto generated docs from Jenkins"
            '''
            withCredentials([gitUsernamePassword(credentialsId: 'stan-github', gitToolName: 'git-tool')]) {
              sh "git push --force origin HEAD:$branch"
            }
          }
        }
      }
    }
  }
}

emailFailure()
