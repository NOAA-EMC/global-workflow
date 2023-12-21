pipeline {
    agent{ label 'orion-emc'}

    stages {    
        stage('Checkout') {
            steps {
                checkout scm
                script {
                   pullRequest.removeLabel('CI-Orion-Ready')
                   pullRequest.addLabel('CI-Orion-Building')
                }
                sh 'git submodule update --init --recursive'
            }
        }
        stage('Build') {
          steps {
            sh 'sorc/build_all.sh'
            sh 'sorc/link_workflow.sh'
          }
        }
        stage ('Create Experment') {
            steps {
                script {
                  pullRequest.removeLabel('CI-Orion-Building')
                  pullRequest.addLabel('CI-Orion-Running')  
                }
                sh '''
                   export HOMEgfs=$PWD
                   mkdir -p RUNTESTS
                   export RUNTESTS=$PWD/RUNTESTS
                   source ci/platforms/config.orion
                   pr_sha=$(git rev-parse --short HEAD)
                   case=C48_ATM
                   export pslot=${case}_${pr_sha}
                   source workflow/gw_setup.sh
                   workflow/create_experiment.py --yaml ci/cases/pr/${case}
                   '''
                script {   
                  pullRequest.comment('SUCCESS creating experment C48_ATM on Orion') 
                }
            }
        }
    }

    post {
        success {
            script {
                pullRequest.removeLabel('CI-Orion-Running')
                pullRequest.addLabel('CI-Orion-Passed')  
            }
        }
        failure {
            script {
                pullRequest.removeLabel('CI-Orion-Running')
                pullRequest.addLabel('CI-Orion-Failed')  
            }
        }
    }
}
