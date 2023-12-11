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
                withCredentials([string(credentialsId: '7dcc5bf3-cc6a-48b1-acd8-c1dc35e3b10b', variable: 'GITHUB_TOKEN')])
                pullRequest.labels.add('CI-Orion-Passed')
            }
        }
    }
}