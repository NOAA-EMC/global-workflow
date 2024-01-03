pipeline {
    agent none
    stages {
        stage('BuildAndTest') {
            matrix {
                agent{ label 'orion-emc'}
                axes {
                    axis {
                        name 'PLATFORM'
                        values 'Orion', 'Hera', 'Hercules'
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
                    stage('Run Tests') {
                        steps {
                            echo "Do Test for ${PLATFORM} - ${Cases}"
                        }
                    }
                }
            }
        }
    }
}