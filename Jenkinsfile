pipeline {
   agent{ label 'demoJNLP'}

    stages {    
        stage('Checkout Repos') {
            steps {
                sh './sorc/checkout.sh -c -g -u'
            }
            githubPRAddLabels labelProperty: labels('CI-Orion-Built')
        }
    }
}
