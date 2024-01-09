pipeline {

    agent none
    options {
        disableConcurrentBuilds(abortPrevious: true)
        skipDefaultCheckout()
        buildDicarder(logRotator(numToKeepStr: '2'))
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
                }
                echo "Do Build for ${machine}"
                checkout scm
                script { env.MACHINE_ID = MACHINE }
                // sh 'sorc/build_all.sh -gu'
                sh 'sorc/link_workflow.sh'
            }
        }

        stage('Build and Test') {
            when {
                expression { MACHINE != 'none' }
            }

            matrix {
                agent { label "${MACHINE}-emc" }
                axes {
                    axis {
                        name 'Cases'
                        values 'C48_ATM', 'C48_S2SWA_gefs', 'C48_S2SW', 'C96_atm3DVar'
                    }
                }
                stages {
                    stage('Create Experiment') {
                        steps {
                            script {
                                env.case = "${Cases}"
                                env.RUNTESTS = "${WORKSPACE}/RUNTESTS"
                            }
                            sh 'mkdir -p ${RUNTESTS}'
                            sh '${WORKSPACE}/ci/scripts/utils/ci_utils_wrapper.sh create_experiment ci/cases/pr/${case}.yaml'
                        }
                    }
                    stage("Run Cases") {
                        steps {
                            echo "Do Test for ${machine} - ${Cases}"
                        }
                    }
                }
            }
        }

    }
}