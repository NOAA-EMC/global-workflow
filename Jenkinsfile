pipeline {
   agent{ label 'demoJNLP'}

    stages {    
        stage('Checkout Repos') {
            steps {
                sh './source/checkout.sh -c -g -u'
            }

        }
    }
}