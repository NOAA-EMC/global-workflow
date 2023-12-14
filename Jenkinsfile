pipeline {
    agent{ label 'local'}

    stages {    
        stage('Checkout') {
        node{ 
          checkout scm
        }
    }
    post {
        success {
            script {
                pullRequest.labels.add('CI-Orion-Passed')
            }
        }
    }
}
