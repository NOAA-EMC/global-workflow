pipeline {
    agent none

    options {
        disableConcurrentBuilds()
        overrideIndexTriggers(false)
        skipDefaultCheckout(true)
    }

    stages {

        stage( 'Get Machine' ) {

            steps {
                script {
                    for (label in pullRequest.labels) {
                        if ((label.matches("CI-Hera-Ready"))) {
                             env.MACHINE='hera'
                        }  
                        else if ((label.matches("CI-Orion-Ready"))) {
                            env.MACHINE='orion'
                        }  
                        else if ((label.matches("CI-Hercules-Ready"))) {
                            env.MACHINE='hercules'
                        }  
                        else { 
                            env.MACHINE='none'
                        }
                     }
                }
            }
        }

        stage( 'Build and Test' ) {

            when {
                expression { env.MACHINE != 'none' }
            }

            matrix {
                agent { label "${MACHINE}-emc" }
                axes {
                    axis {
                        name 'Cases'
                        values 'C48_ATM', 'C48_S2SWA_gefs', 'C48_S2SW',  'C96_atm3DVar', 'C96C48_hybatmDA'
                    }
                }
                stages {
                    stage('Build') {
                        steps {
                           echo "Do Build for ${MACHINE^} - ${Cases}"
                           script {
                            pullRequest.removeLabel('CI-${MACHINE^}-Ready')
                            pullRequest.addLabel('CI-${MACHINE^}-Building')
                           }
                            cleanWs()
                            checkout scm
                            //sh 'sorc/build_all.sh -gu'
                            sh 'sorc/link_workflow.sh'
                        }
                    }
                    stage('Create Experiment') {
                        steps {
                            script { 
                                env.case = ${Cases}
                                env.RUNTESTS = "${WORKSPACE}/RUNTESTS"
                            }
                            sh '''
                                rm -rf ${RUNTESTS}
                                mkdir -p ${RUNTESTS}
                                '''
                            sh '${WORKSPACE}/ci/scripts/utils/ci_utils_wrapper.sh create_experiment ci/cases/pr/${case}.yaml'
                        }
                    }
                    stage("Run ${Cases}") {
                        steps {
                            echo "Do Test for ${CHOICE_NODE} - ${Cases}"
                        }
                    }
                }
            }
        }
    }

    post {
        always {
            script {
                pullRequest.removeLabel('CI-${MACHINE^}-Building')
                pullRequest.addLabel('CI-${MACHINE^}-Done')
            }
        }
    }
}