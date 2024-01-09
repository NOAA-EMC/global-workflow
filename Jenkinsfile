pipeline {
    agent any

    stages {
        stage('Build') {
            when {
                expression { env.MACHINE != 'none' }
            }
            steps {
                script {
                    machine = env.MACHINE[0].toUpperCase() + env.MACHINE.substring(1)
                    pullRequest.removeLabel("CI-${machine}-Ready")
                    pullRequest.addLabel("CI-${machine}-Building")
                }
                echo "Do Build for ${machine}"
                checkout scm
                sh 'sorc/build_all.sh -gu'
                script { env.MACHINE_ID = env.MACHINE } 
                sh 'sorc/link_workflow.sh'
            }
        }

        stage('Build and Test') {
            when {
                expression { env.MACHINE != 'none' }
            }

            matrix {
                agent { label "${env.MACHINE}-emc" }
                axes {
                    axis {
                        name 'Cases'
                        values 'C48_ATM', 'C48_S2SWA_gefs', 'C48_S2SW',  'C96_atm3DVar'
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