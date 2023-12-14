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
                pullRequest.addLabel(['CI-Orion-Passed'])  
            }
        }
    }
}
