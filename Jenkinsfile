pipeline {
    agent{ label 'Sanbox'}

    stages {    
        stage('Checkout Repos') {
            steps {
                sh './sorc/checkout.sh -c -g -u'
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