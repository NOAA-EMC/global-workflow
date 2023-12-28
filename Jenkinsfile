
def cases="cases"
pipeline {
    agent{ label 'orion-emc'}

    environment {
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
            sh 'sorc/build_all.sh'
            sh 'sorc/link_workflow.sh'
            script {
              case_list_output = sh( script: "${WORKSPACE}/ci/scripts/utils/ci_utils_wrapper.sh get_pr_case_list", returnStdout: true ).trim()
              cases = case_list_output.tokenize('\n').remove(0)
              echo "cases: ${cases}"
            }
          }
        }
 
        stage('Create Cases') {
        agent{ label 'orion-emc'}
            steps {
                sh '''
                   rm -rf ${RUNTESTS}
                   mkdir -p ${RUNTESTS}
                   '''
                script {
                    pullRequest.removeLabel('CI-Orion-Building')
                    pullRequest.addLabel('CI-Orion-Running')
                    echo "cases: ${cases}"
                    cases.each { case_name ->
                        stage("Create ${case_name}") {
                            script {
                                env.case = case_name
                            }
                            sh '''
                               source ci/platforms/config.orion
                               pr_sha=$(git rev-parse --short HEAD)
                               export pslot=${case}_${pr_sha}
                               source workflow/gw_setup.sh
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