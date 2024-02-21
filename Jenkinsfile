def Machine = 'none'
def machine = 'none'
def HOME = 'none'
def localworkspace = 'none'
def commonworkspace = 'none'

pipeline {
    agent { label 'built-in' }

    options {
        skipDefaultCheckout()
        parallelsAlwaysFailFast()
    }

    stages { // This initial stage is used to get the Machine name from the GitHub labels on the PR
             // which is used to designate the Nodes in the Jenkins Controler by the agent label
             // Each Jenknis Node is connected to said machine via an JAVA agent via an ssh tunnel

        stage('Get Machine') {
            agent { label 'built-in' }
            steps {
                script {
                    localworkspace = env.WORKSPACE
                    machine = 'none'
                    for (label in pullRequest.labels) {
                        echo "Label: ${label}"
                        if ((label.matches('CI-Hera-Ready'))) {
                            machine = 'hera'
                        } else if ((label.matches('CI-Orion-Ready'))) {
                            machine = 'orion'
                        } else if ((label.matches('CI-Hercules-Ready'))) {
                            machine = 'hercules'
                        }
                    } // createing a second machine varible with first letter capital
                      // because the first letter of the machine name is captitalized in the GitHub labels
                    Machine = machine[0].toUpperCase() + machine.substring(1)
                }
            }
        }

        stage('Get Common Workspace') {
            agent { label "${machine}-emc" }
            steps {
                script {
                    properties([parameters([[$class: 'NodeParameterDefinition', allowedSlaves: ['built-in', 'Hera-EMC', 'Orion-EMC'], defaultSlaves: ['built-in'], name: '', nodeEligibility: [$class: 'AllNodeEligibility'], triggerIfResult: 'allCases']])])
                    HOME = "${WORKSPACE}/TESTDIR"
                    commonworkspace = "${WORKSPACE}"
                    sh(script: "mkdir -p ${HOME}/RUNTESTS")
                    pullRequest.addLabel("CI-${Machine}-Building")
                    if (pullRequest.labels.any { value -> value.matches("CI-${Machine}-Ready") }) {
                        pullRequest.removeLabel("CI-${Machine}-Ready")
                    }
                }
            }
        }

        stage('Build System') {
            matrix {
                agent { label "${machine}-emc" }
                //options {
                //    throttle(['global_matrix_build'])
                //}
                axes {
                    axis {
                        name 'system'
                        values 'gfs', 'gefs'
                    }
                }
                stages {
                    stage('build system') {
                        steps {
                            script {
                                def HOMEgfs = "${HOME}/${system}" // local HOMEgfs is used to build the system on per system basis under the common workspace HOME
                                sh(script: "mkdir -p ${HOMEgfs}")
                                ws(HOMEgfs) {
                                    env.MACHINE_ID = machine // MACHINE_ID is used in the build scripts to determine the machine and is added to the shell environment
                                    if (fileExists("${HOMEgfs}/sorc/BUILT_semaphor")) { // if the system is already built, skip the build in the case of re-runs
                                        sh(script: "cat ${HOMEgfs}/sorc/BUILT_semaphor", returnStdout: true).trim() // TODO: and user configurable control to manage build semphore
                                        pullRequest.comment("Cloned PR already built (or build skipped) on ${machine} in directory ${HOMEgfs}<br>Still doing a checkout to get the latest changes")
                                        sh(script: 'source workflow/gw_setup.sh; git pull --recurse-submodules')
                                        dir('sorc') {
                                            sh(script: './link_workflow.sh')
                                        }
                                    } else {
                                        checkout scm
                                        sh(script: 'source workflow/gw_setup.sh;which git;git --version;git submodule update --init --recursive')
                                        def builds_file = readYaml file: 'ci/cases/yamls/build.yaml'
                                        def build_args_list = builds_file['builds']
                                        def build_args = build_args_list[system].join(' ').trim().replaceAll('null', '')
                                        dir("${HOMEgfs}/sorc") {
                                            sh(script: "${build_args}")
                                            sh(script: './link_workflow.sh')
                                            sh(script: "echo ${HOMEgfs} > BUILT_semaphor")
                                        }
                                    }
                                    if (env.CHANGE_ID && system == 'gfs') {
                                       if (pullRequest.labels.any { value -> value.matches("CI-${Machine}-Building") }) {
                                           pullRequest.removeLabel("CI-${Machine}-Building")
                                       }
                                       pullRequest.addLabel("CI-${Machine}-Running")
                                    }
                               }
                           }
                        }
                    }
                }
            }
        }

        stage('Run Tests') {
            matrix {
                agent { label "${machine}-emc" }
                axes {
                    axis {
                        name 'Case'
                        // TODO add dynamic list of cases from env vars (needs addtional plugins)
                        values 'C48_ATM', 'C48_S2SWA_gefs', 'C48_S2SW', 'C96_atm3DVar', 'C96C48_hybatmDA', 'C96_atmsnowDA'  // 'C48mx500_3DVarAOWCDA'
                    }
                }
                stages {
                    stage('Create Experiment') {
                        steps {
                                script {
                                    sh(script: "sed -n '/{.*}/!p' ${HOME}/gfs/ci/cases/pr/${Case}.yaml > ${HOME}/gfs/ci/cases/pr/${Case}.yaml.tmp")
                                    def yaml_case = readYaml file: "${HOME}/gfs/ci/cases/pr/${Case}.yaml.tmp"
                                    system = yaml_case.experiment.system
                                    def HOMEgfs = "${HOME}/${system}"   // local HOMEgfs is used to populate the XML on per system basis
                                    env.RUNTESTS = "${HOME}/RUNTESTS"
                                    sh(script: "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh create_experiment ${HOMEgfs}/ci/cases/pr/${Case}.yaml")
                                }
                        }
                    }
                    stage('Run Experiments') {
                        steps {
                            script {
                                HOMEgfs = "${HOME}/gfs"  // common HOMEgfs is used to launch the scripts that run the experiments
                                ws(HOMEgfs) {
                                    pslot = sh(script: "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot ${HOME}/RUNTESTS ${Case}", returnStdout: true).trim()
                                    // pullRequest.comment("**Running** experiment: ${Case} on ${Machine}<br>With the experiment in directory:<br>`${HOME}/RUNTESTS/${pslot}`")
                                    err =  sh(script: "${HOMEgfs}/ci/scripts/run-check_ci.sh ${HOME} ${pslot}")
                                    if (err != 0) {
                                        pullRequest.comment("**FAILURE** running experiment: ${Case} on ${Machine}")
                                        sh(script: "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh cancel_all_batch_jobs ${HOME}/RUNTESTS")
                                        ws(HOME) {
                                            if (fileExists('RUNTESTS/error.logs')) {
                                                def fileContent = readFile 'RUNTESTS/error.logs'
                                                def lines = fileContent.readLines()
                                                for (line in lines) {
                                                    echo "archiving: ${line}"
                                                    archiveArtifacts artifacts: "${line}", fingerprint: true
                                                }   
                                            }
                                        }
                                        error("Failed to run experiments ${Case} on ${Machine}")
                                    }
                                   // pullRequest.comment("**SUCCESS** running experiment: ${Case} on ${Machine}")
                                }
                            }

                        }
                    }
                }
            }
        }
    }

    post {
        always {
            script {
                if(env.CHANGE_ID) {
                    for (label in pullRequest.labels) {
                        if (label.contains("${Machine}")) {
                            pullRequest.removeLabel(label)
                        }
                    }
                }
            }
        }
        success {
            script {
                if(env.CHANGE_ID) {
                    pullRequest.addLabel("CI-${Machine}-Passed")
                    def timestamp = new Date().format('MM dd HH:mm:ss', TimeZone.getTimeZone('America/New_York'))
                    pullRequest.comment("**CI SUCCESS** ${Machine} at ${timestamp}\n\nBuilt and ran in directory `${HOME}`")
                }
            }
        }
        failure {
            script {
                if(env.CHANGE_ID) {
                    pullRequest.addLabel("CI-${Machine}-Failed")
                    def timestamp = new Date().format('MM dd HH:mm:ss', TimeZone.getTimeZone('America/New_York'))
                    pullRequest.comment("**CI FAILED** ${Machine} at ${timestamp}<br>Built and ran in directory `${HOME}`")
                }
            }
        }
    }
}
