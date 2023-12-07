pipeline {
    agent DemoJNLP

    environment {
        REPO_URL = 'https://github.com/TerrenceMcGuinness-NOAA/global-workflow.git'
        PR_NUMBER = '203'
        GITHUB_TOKEN = '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'
    }


    stage('Checkout') {
       steps {
              git branch: "refs/pull/${PR_NUMBER}/head", url: "${REPO_URL}"
             }
        }

        stage('Build') {
            steps {
                sh 'touch this_examle'
            }
        }
    }
