def MACHINE = 'none'
def machine = 'none'
def HOMEgfs = 'none'

pipeline {
    agent { label 'built-in' }

    options {
        disableConcurrentBuilds(abortPrevious: true)
        skipDefaultCheckout(true)
        buildDiscarder(logRotator(numToKeepStr: '2'))
    }

    stages {

        stage('Get Machine') {
            agent { label 'built-in' }
            steps {
                script {
                    MACHINE = 'none'
                    for (label in pullRequest.labels) {
                        echo "Label: ${label}"
                        if ((label.matches("CI-Hera-Ready"))) {
                            MACHINE = 'hera'
                        } else if ((label.matches("CI-Orion-Ready"))) {
                            MACHINE = 'orion'
                        } else if ((label.matches("CI-Hercules-Ready"))) {
                            MACHINE = 'hercules'
                        }
                    }
                    machine = MACHINE[0].toUpperCase() + MACHINE.substring(1)
                }
            }
        }

        stage('Build') {
            agent { label "${MACHINE}-emc" }
            //when {
            //    expression { MACHINE != 'none' }
            //}
            steps {
                script {
                properties([parameters([[$class: 'NodeParameterDefinition', allowedSlaves: ['ALL (no restriction)', 'built-in','Hera-EMC','Orion-EMC'], defaultSlaves: ['Hera-EMC'], name: '', nodeEligibility: [$class: 'AllNodeEligibility'], triggerIfResult: 'allCases']])])
                    //pullRequest.removeLabel("CI-${machine}-Ready")
                    pullRequest.addLabel("CI-${machine}-Building")
                    checkout scm
                    HOMEgfs = "${WORKSPACE}"
                }
                script {
                    env.MACHINE_ID = MACHINE
                    if (fileExists("${HOMEgfs}/sorc/BUILT_sema")) {
                        HOMEgfs = sh( script: "cat ${HOMEgfs}/sorc/BUILT_sema", returnStdout: true).trim()
                        pullRequest.comment("Cloned PR already built (or build skipped) on ${machine} in directory ${HOMEgfs}")
                    }
                    else {
                        //sh( script: "sorc/build_all.sh -gu", returnStatus: false)
                        sh( script: "sorc/build_all_stub.sh" )
                        sh( script: "rm -Rf ${WORKSPACE}/RUNTESTS", returnStatus: true)
                        sh( script: "mkdir -p ${WORKSPACE}/RUNTESTS", returnStatus: true)
                        sh( script: "echo ${HOMEgfs} > ${HOMEgfs}/sorc/BUILT_sema", returnStatus: true)
                    }
                    sh( script: "sorc/link_workflow.sh", returnStatus: false)
                    //pullRequest.removeLabel("CI-${machine}-Building")
                    pullRequest.addLabel("CI-${machine}-Running")
                }
            }
        }

        stage('Run Tests') {
            //when {
            //    expression { MACHINE != 'none' }
            //}
            matrix {
                agent { label "${machine}-emc" }
                axes {
                    axis {
                        name "Case"
                        //values "C48_ATM", "C48_S2SWA_gefs", "C48_S2SW", "C96_atm3DVar"
                        values "C48_ATM", "C48_S2SW"
                    }
                }
                stages {
                    stage('Create Experiment') {
                        steps {
                            ws(HOMEgfs) {
                                script {
                                    env.RUNTESTS = "${HOMEgfs}/RUNTESTS"
                                    sh( script: "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh create_experiment ${HOMEgfs}/ci/cases/pr/${Case}.yaml", returnStatus: false)
                                }
                            }
                        }
                    }
                    stage('Run Experiments') {
                        steps {
                            ws(HOMEgfs) {
                                script {
                                    pslot = sh( script: "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot ${HOMEgfs}/RUNTESTS ${Case}", returnStdout: true ).trim()
                                    pullRequest.comment("Running experiments: ${Case} with pslot ${pslot} on ${machine}")
                                    //sh( script: "${HOMEgfs}/ci/scripts/run-check_ci.sh ${HOMEgfs} ${pslot}", returnStatus: false)
                                    sh( script: "${HOMEgfs}/ci/scripts/run-check_ci_stub.sh ${HOMEgfs} ${pslot}")
                                    pullRequest.comment("SUCCESS running experiments: ${Case} on ${machine}")
                               }
                            }
                        }
                    }
                }
            }
       }

    }

    post {
        success {
            script {
                if ( pullRequest.labels.contains( "CI-${machine}-Running" ) ) {
                   pullRequest.removeLabel("CI-${machine}-Running")
                }
                pullRequest.addLabel("CI-${machine}-Passed")
                def timestamp = new Date().format("MM dd HH:mm:ss", TimeZone.getTimeZone('America/New_York'))
                pullRequest.comment("SUCCESSFULLY ran all CI Cases on ${machine} at ${timestamp}")
            }
            cleanWs()
        }
        failure {
            script {
                if(pullRequest.labels.contains("CI-${machine}-Running")) {
                   pullRequest.removeLabel("CI-${machine}-Running")
                } 
                pullRequest.addLabel("CI-${machine}-Failed")
                def timestamp = new Date().format("MM dd HH:mm:ss", TimeZone.getTimeZone('America/New_York'))
                pullRequest.comment("CI FAILED ${machine} at ${timestamp}\n\nBuilt and ran in directory ${HOME}")
            }
        }
    }

}
