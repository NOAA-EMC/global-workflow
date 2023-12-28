
def cases="cases"
pipeline {
    agent{ label 'orion-emc'}

    environment {
        HOMEgfs = "${WORKSPACE}"
        RUNTESTS = "${WORKSPACE}/RUNTESTS"
    }

    stages {    
        stage('Checkout') {
        agent{ label 'orion-emc'}
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
        agent{ label 'orion-emc'}
          steps {
            // sh 'sorc/build_all.sh'
            sh 'sorc/link_workflow.sh'
            script {
              case_list_output = sh( script: "${WORKSPACE}/ci/scripts/utils/ci_utils_wrapper.sh get_pr_case_list", returnStdout: true ).trim()
              case_list = case_list_output.tokenize('\n')
              case_list.remove(0)
              cases=case_list
              echo "cases: ${cases}"
            }
          }
        }
 
        stage('Create Cases') {
        agent{ label 'orion-emc'}
            steps {
                script {
                    pullRequest.removeLabel('CI-Orion-Building')
                    pullRequest.addLabel('CI-Orion-Running')
                    echo "cases: ${cases}"
                    cases.each { case_name ->
                        stage("Run ${case_name}") {
                            script {
                               env.case = case_name
                             }
                              sh '''
                              mkdir -p ${RUNTESTS}
                              source ci/platforms/config.orion
                              pr_sha=$(git rev-parse --short HEAD)
                              export pslot=${case}_${pr_sha}
                              source workflow/gw_setup.sh
                              unset HOMEgfs
                              HOMEgfs=${WORKSPACE}
                              workflow/create_experiment.py --yaml ci/cases/pr/${case}.yaml
                              '''
                              script {
                               pullRequest.comment("SUCCESS creating ${case_name} on Orion")
                              }
                        }
                    }
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