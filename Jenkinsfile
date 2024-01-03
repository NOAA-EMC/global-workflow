pipeline {
    agent{ label 'orion-emc'}
    stages {
        stage('BuildAndTest') {
            matrix {
                axes {
                    axis {
                        name 'PLATFORM'
                        values 'Orion', 'Hera'
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