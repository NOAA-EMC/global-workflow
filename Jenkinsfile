pipeline {
    agent{ label 'demoJNLP'}

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