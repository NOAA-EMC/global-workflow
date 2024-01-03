pipeline {
    agent none
    stages {
        stage('BuildAndTest') {
            matrix {
                agent { label "${PLATFORM}-emc" }
                axes {
                    axis {
                        name 'PLATFORM'
                        values 'orion', 'hera'
                    }
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