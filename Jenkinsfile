pipeline {
    agent{ label 'orion-emc'}

    stages {    
        stage('Checkout') {
            steps {
                checkout scm
                sh 'git submodule update --init --recursive'
            }
        }
        stage('Build') {
          steps {
            sh 'sorc/build_all.sh'
          }
        }
        stage ('Create Experment') {
            steps {
               sh '''
                  export HOMEgfs=$PWD
                  mkdir -p RUNTESTS
                  export RUNTESTS=$PWD/RUNTESTS
                  source ci/platforms/config.orion.sh
                  pr_sha=$(git rev-parse --short HEAD)
                  case=C48_ATM
                  export pslot=${case}_${pr_sha}
                  workflow/create_experment.py --yaml ci/cases/pr/${case}'
                  '''
            }
        }
    }

    post {
        success {
            script {
                pullRequest.addLabel(['CI-Orion-Passed'])  
            }
        }
    }
}
