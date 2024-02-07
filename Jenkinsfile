def Machine = 'none'
def machine = 'none'
def HOME = 'none'
def localworkspace = 'none'

pipeline {
    agent { label 'built-in' }

    options {
        skipDefaultCheckout()
        buildDiscarder(logRotator(numToKeepStr: '2'))
    }

    stages {

        stage('Get Machine') {
            agent { label 'built-in' }
            steps {
                script {
                    localworkspace = env.WORKSPACE
                    machine = 'none'
                    for (label in pullRequest.labels) {
                        echo "Label: ${label}"
                        if ((label.matches("CI-Hera-Ready"))) {
                            machine = 'hera'
                        } else if ((label.matches("CI-Orion-Ready"))) {
                            machine = 'orion'
                        } else if ((label.matches("CI-Hercules-Ready"))) {
                            machine = 'hercules'
                        }
                    }
                    Machine = machine[0].toUpperCase() + machine.substring(1)
                }
            }
        }

        stage('Get Common Workspace') {
            agent { label "${machine}-emc" }
            steps ( timeout(time: 1, unit: 'HOURS') ) {
                script {
                    properties([parameters([[$class: 'NodeParameterDefinition', allowedSlaves: ['built-in','Hera-EMC','Orion-EMC'], defaultSlaves: ['built-in'], name: '', nodeEligibility: [$class: 'AllNodeEligibility'], triggerIfResult: 'allCases']])])
                    HOME = "${WORKSPACE}/TESTDIR"
                    commonworkspace = "${WORKSPACE}"
                    sh( script: "mkdir -p ${HOME}/RUNTESTS", returnStatus: true)
                    pullRequest.addLabel("CI-${Machine}-Building")
                    if ( pullRequest.labels.any{ value -> value.matches("CI-${Machine}-Ready") } ) {
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
                        name "system"
                        values "gfs", "gefs"
                    }
                }
                stages {
                    stage("build system") {
                        steps {
                            script {
                                def HOMEgfs = "${HOME}/${system}"
                                sh( script: "mkdir -p ${HOMEgfs}", returnStatus: true)
                                ws(HOMEgfs) {
                                    env.MACHINE_ID = machine
                                    if (fileExists("${HOMEgfs}/sorc/BUILT_semaphor")) {
                                        sh( script: "cat ${HOMEgfs}/sorc/BUILT_semaphor", returnStdout: true).trim()
                                        ws(localworkspace) { pullRequest.comment("Cloned PR already built (or build skipped) on ${machine} in directory ${HOMEgfs}") }
                                    } else {
                                        checkout scm
                                        sh( script: "source workflow/gw_setup.sh;which git;git --version;git submodule update --init --recursive", returnStatus: true)
                                        def builds_file = readYaml file: "ci/cases/yamls/build.yaml"
                                        def build_args_list = builds_file['builds']
                                        def build_args = build_args_list[system].join(" ").trim().replaceAll("null", "")
                                        dir("${HOMEgfs}/sorc") {
                                            sh( script: "${build_args}", returnStatus: true)
                                            sh( script: "./link_workflow.sh", returnStatus: true)
                                            sh( script: "echo ${HOMEgfs} > BUILT_semaphor", returnStatus: true)
                                        }
                                    }
                                
                                    if ( pullRequest.labels.any{ value -> value.matches("CI-${machine}-Building") } ) {
                                         pullRequest.removeLabel("CI-${machine}-Building")
                                    }
                                    pullRequest.addLabel("CI-${machine}-Running")
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
                        name "Case"
                        // values "C48_ATM", "C48_S2SWA_gefs", "C48_S2SW", "C96_atm3DVar"
                        values "C48_S2SWA_gefs", "C96_atm3DVar"
                    }
                }
                stages {
                    stage('Create Experiment') {
                        steps { 
                                script {
                                    sh( script: "sed -n '/{.*}/!p' ${HOME}/gfs/ci/cases/pr/${Case}.yaml > ${HOME}/gfs/ci/cases/pr/${Case}.yaml.tmp", returnStatus: true)
                                    def yaml_case = readYaml file: "${HOME}/gfs/ci/cases/pr/${Case}.yaml.tmp"
                                    system = yaml_case.experiment.system
                                    def HOMEgfs = "${HOME}/${system}"
                                    env.RUNTESTS = "${HOME}/RUNTESTS"
                                    sh( script: "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh create_experiment ${HOMEgfs}/ci/cases/pr/${Case}.yaml", returnStatus: true)
                                } 
                        }
                    }
                    stage('Run Experiments') {
                        steps {
                            script {
                                def HOMEgfs = "${HOME}/gfs"
                                ws(HOMEgfs) {
                                   pslot = sh( script: "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot ${HOME}/RUNTESTS ${Case}", returnStdout: true ).trim()
                                   pullRequest.comment("Running experiments: ${Case} with pslot ${pslot} on ${machine}")
                                   try {
                                      sh( script: "${HOMEgfs}/ci/scripts/run-check_ci.sh ${HOME} ${pslot}", returnStatus: true)
                                      pullRequest.comment("SUCCESS running experiments: ${Case} on ${machine}")
                                    } catch (Exception e) {
                                       pullRequest.comment("FAILURE running experiments: ${Case} on ${machine}")
                                       error("Failed to run experiments ${Case} on ${machine}")
                                    }
                                }
                            } 
                        }
                        post {
                            always {
                                script {
                                    ws (localworkspace) {
                                        for (label in pullRequest.labels) {
                                           if (label.contains("${machine}")) {
                                               pullRequest.removeLabel(label)
                                            }
                                        }
                                    }
                               }
                            }
                            success {
                                script {
                                    ws (localwospace) {
                                       pullRequest.addLabel("CI-${machine}-Passed")
                                       def timestamp = new Date().format("MM dd HH:mm:ss", TimeZone.getTimeZone('America/New_York'))
                                       pullRequest.comment("CI SUCCESS ${machine} at ${timestamp}\n\nBuilt and ran in directory ${HOME}")
                                    }
                                }
                            }
                            failure {
                                script {
                                    ws (localworkspace) {
                                    pullRequest.addLabel("CI-${machine}-Failed")
                                    def timestamp = new Date().format("MM dd HH:mm:ss", TimeZone.getTimeZone('America/New_York'))
                                    pullRequest.comment("CI FAILED ${machine} at ${timestamp}\n\nBuilt and ran in directory ${HOME}")
                                    if (fileExists('${HOME}/RUNTESTS/ci.log')) {
                                        def fileContent = readFile '${HOME}/RUNTESTS/ci.log'
                                        fileContent.eachLine { line ->
                                            if( line.contains(".log")) {
                                                archiveArtifacts artifacts: "${line}", fingerprint: true
                                            }
                                        }
                                    }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

}
