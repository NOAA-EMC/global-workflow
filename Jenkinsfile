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
                    echo pullRequest.labels
                    for (label in pullRequest.labels) {
                        echo "Label: ${label}"
                        if ((label.matches("CI-Hera-Ready"))) {
                             MACHINE='hera'
                        }  
                        else if ((label.matches("CI-Orion-Ready"))) {
                            MACHINE='orion'
                        }  
                        else if ((label.matches("CI-Hercules-Ready"))) {
                            MACHINE='hercules'
                        }  
                        else { 
                            MACHINE='none'
                        }
                     }
                }
            }
        }

        stage( 'Build and Test' ) {

            when {
                expression { MACHINE != 'none' }
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
                           script {
                            machine = env.MACHINE[0].toUpperCase() + env.MACHINE.substring(1)
                            pullRequest.removeLabel("CI-${machine}-Ready")
                            pullRequest.addLabel("CI-${machine}-Building")
                           }
                           echo "Do Build for ${machine} ${Cases}"
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
                    stage("Run Cases") {
                        steps {
                            echo "Do Test for ${machine} - ${Cases}"
                        }
                    }
                }
            }
        }
    }

    post {
        always {
            script {
                machine = MACHINE[0].toUpperCase() + MACHINE.substring(1)
                echo "Do Post for ${machine}"
                pullRequest.removeLabel("CI-${machine}-Building")
                pullRequest.addLabel("CI-${machine}-Passed")
                echo "no-op"
            }
        }
    }
}