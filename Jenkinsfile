pipeline {
    agent{ label 'local'}

    stages {    
        stage('Checkout') {
            steps { 
                checkout scm
            }
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
