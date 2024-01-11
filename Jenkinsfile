pipeline {

    agent none
    options {
        disableConcurrentBuilds(abortPrevious: true)
        skipDefaultCheckout()
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
                }
                echo "Do Build for ${machine}"
                checkout scm
                script {
                    HOME = "${WORKSPACE}"
                    env.MACHINE_ID = MACHINE
                }
                sh 'sorc/build_all.sh -gu'
                sh 'sorc/link_workflow.sh'
                sh 'mkdir -p ${WORKSPACE}/RUNTESTS'
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
                        name "Cases"
                        values "C48_ATM", "C48_S2SWA_gefs", "C48_S2SW", "C96_atm3DVar"
                    }
                }
                stages {
                    stage('Create Experiment') {
                        steps {
                            script {
                                 env.HOME = "$HOME"
                                 env.RUNTESTS = "${HOME}/RUNTESTS"
                            }
                            echo "Cases: ${Cases} ${HOME}"
                            sh '${HOME}/ci/scripts/utils/ci_utils_wrapper.sh create_experiment ${HOME}/ci/cases/pr/${Cases}.yaml'
                        }
                    }

                    stage('Run Experiments') {
                        steps {
                            script {
                                pslot = sh( script: "${HOME}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot ${HOME}/RUNTESTS ${Case}", returnStdout: true ).trim()
                                //pullRequest.removeLabel('CI-${machine}-Building')
                                //pullRequest.addLabel('CI-${machine}-Running')
                            }
                            sh '${WORKSPACE}/ci/scripts/run-check_ci.sh ${HOME} ${pslot}'
                            script { pullRequest.comment("SUCCESS running experiments: ${Case} on Orion") }
                        }
                    }
                }
            }
        }
    }

    post {
        success {
            script {
                echo "SUCCESS"
                //pullRequest.removeLabel('CI-${machine}-Running')
                //pullRequest.addLabel('CI-${machine}-Passed')  
            }
        }
        failure {
            script {
                echo "FAILURE"
                //pullRequest.removeLabel('CI-${machine}-Running')
                //pullRequest.addLabel('CI-${machine}-Failed')  
            }
        }
    }

}
