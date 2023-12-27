pipeline {
    agent{ label 'orion-emc'}

    environment {
        HOMEgfs = "${WORKSPACE}"
        RUNTESTS = "${WORKSPACE}/RUNTESTS"
    }

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
            env.CASE_LIST = sh( script: "${WORKSPACE}/ci/scripts/utils/ci_utils_wrapper.sh get_pr_case_list", returnStdout: true ).trim()
          }
        }

        def cases = env.CASE_LIST.tokenize('\n')

        cases.each { case_name ->
            stage("Run ${case_name}") {
                steps {
                    sh '''
                       export HOMEgfs=${env.HOMEgfs}
                       mkdir -p ${env.RUNTESTS}
                       export RUNTESTS=${env.RUNTESTS}
                       source ci/platforms/config.orion
                       pr_sha=$(git rev-parse --short HEAD)
                       case=${case_name}
                       export pslot=${case}_${pr_sha}
                       source workflow/gw_setup.sh
                       workflow/create_experiment.py --yaml ci/cases/pr/${case}.yaml
                       '''
                    script {
                        pullRequest.comment("SUCCESS creating ${case_name} on Orion")
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
