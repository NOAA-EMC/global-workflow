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
                }
            }
        }

        stage('Build') {
            agent { label "${MACHINE}-emc" }
            when {
                expression { MACHINE != 'none' }
            }
            steps {
                script {
                    machine = MACHINE[0].toUpperCase() + MACHINE.substring(1)
                    pullRequest.removeLabel("CI-${machine}-Ready")
                    pullRequest.addLabel("CI-${machine}-Building")
                    cleanWs()
                    checkout scm
                    HOMEgfs = "${WORKSPACE}"
                    env.MACHINE_ID = MACHINE
                    //sh( script: "sorc/build_all.sh -gu", returnStatus: false)
                    sh( script: "sorc/link_workflow.sh", returnStatus: false)
                    sh( script: "mkdir -p ${WORKSPACE}/RUNTESTS", returnStatus: false)
                    pullRequest.removeLabel("CI-${machine}-Building")
                    pullRequest.addLabel("CI-${machine}-Running")
                }
            }
        }

        stage('Run Tests') {
            when {
                expression { MACHINE != 'none' }
            }
            matrix {
                agent { label "${MACHINE}-emc" }
                axes {
                    axis {
                        name "Case"
                        values "C48_ATM", "C48_S2SWA_gefs", "C48_S2SW", "C96_atm3DVar"
                    }
                }
                stages {
                    stage('Create Experiment') {
                        steps {
                            ws(HOMEgfs) {
                                script {
                                    env.HOME=HOMEgfs
                                    env.RUNTESTS = "${HOME}/RUNTESTS"
                                    sh( script: "ci/scripts/utils/ci_utils_wrapper.sh create_experiment ${HOME}/ci/cases/pr/${Case}.yaml", returnStatus: false)
                                }
                            }
                        }
                    }
                    stage('Run Experiments') {
                        steps {
                            ws(HOMEgfs) {
                                script {
                                    env.HOME=HOMEgfs
                                    pslot = sh( script: "ci/scripts/utils/ci_utils_wrapper.sh get_pslot ${HOME}/RUNTESTS ${Case}", returnStdout: true ).trim()
                                    pullRequest.comment("Running experiments: ${Case} with pslot ${pslot} on ${machine}")
                                    sh( script: "ci/scripts/run-check_ci.sh ${HOME} ${pslot}", returnStatus: false)
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
                if(pullRequest.labels.contains("CI-${machine}-Running")) {
                   pullRequest.removeLabel('CI-${machine}-Running')
                   pullRequest.addLabel('CI-${machine}-Passed')
                }
                def timestamp = new Date().format("MM dd HH:mm:ss", TimeZone.getTimeZone('America/New_York'))
                pullRequest.comment("SUCCESSFULLY ran all CI Cases on ${machine} at ${timestamp}")
            }
            cleanWs()
        }
        failure {
            script {
                if(pullRequest.labels.contains("CI-${machine}-Running")) {
                   pullRequest.removeLabel('CI-${machine}-Running')
                   pullRequest.addLabel('CI-${machine}-Failed')
                }
                def timestamp = new Date().format("MM dd HH:mm:ss", TimeZone.getTimeZone('America/New_York'))
                pullRequest.comment("CI FAILED ${machine} at ${timestamp}\n\nBuilt and ran in directory ${HOME}")
            }
        }
    }

}
