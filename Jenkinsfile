pipeline {
    agent none
    stages {

        stage( 'Get Machine' ) {

            steps {
                script {
                    for (label in pullRequest.labels) {
                        if ((label.matches("CI-Hera-Ready"))) {
                             env.CHOICE_NODE='hera-emc'
                        }  
                        else if ((label.matches("CI-Orion-Ready"))) {
                            env.CHOICE_NODE='orion-emc'
                        }  
                        else if ((label.matches("CI-Hercules-Ready"))) {
                            env.CHOICE_NODE='hercules-emc'
                        }  
                        else { 
                            env.CHOICE_NODE='none'
                        }
                     }
                }
            }
        }

        stage( 'Build and Test' ) {

            when {
                expression { env.CHOICE_NODE != 'none' }
            }

            matrix {
                agent { label "${CHOICE_NODE}" }
                axes {
                    axis {
                        name 'Cases'
                        values 'C48_ATM', 'C48_S2SWA_gefs', 'C48_S2SW',  'C96_atm3DVar', 'C96C48_hybatmDA'
                    }
                }
                stages {
                    stage('Build') {
                        steps {
                            echo "Do Build for ${PLATFORM}"
                        }
                    }
                    stage('Run Cases') {
                        steps {
                            echo "Do Test for ${PLATFORM} - ${Cases}"
                        }
                    }
                }
            }
        }
    }
}