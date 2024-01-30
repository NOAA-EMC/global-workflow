def MACHINE = 'none'
def machine = 'none'
def HOME = 'none'

pipeline {
    agent { label 'built-in' }

    options {
        disableConcurrentBuilds(abortPrevious: true)
        skipDefaultCheckout(true)
        buildDiscarder(logRotator(numToKeepStr: '3'))
    }

    stages {

        stage('Get Machine') {
            agent { label 'built-in' }
            steps {
                script {
                    MACHINE = 'none'
                    for (label in pullRequest.labels) {
                        echo "Label: ${label}"
                        if ((label.matches("CI-Hera-Ready"))) {
                            MACHINE = 'hera'
                        } else if ((label.matches("CI-Orion-Ready"))) {
                            MACHINE = 'orion'
                        } else if ((label.matches("CI-Hercules-Ready"))) {
                            MACHINE = 'hercules'
                        }
                    }
                    machine = MACHINE[0].toUpperCase() + MACHINE.substring(1)
                }
            }
        }

        stage('Get Common Workspace') {
            agent { label "${MACHINE}-emc" }
            steps ( timeout(time: 1, unit: 'HOURS') ) {
                script {
                    HOME = "${WORKSPACE}/TESTDIR"
                    pullRequest.addLabel("CI-${machine}-Building")
                    if ( pullRequest.labels.any{ value -> value.matches("CI-${machine}-Ready") } ) {
                        pullRequest.removeLabel("CI-${machine}-Ready")
                    }
                }
            }
        }

        stage('Build GEFS') {
            matrix {
                agent { label "${MACHINE}-emc" }
                //options {
                //    throttle(['Matrix_build'])
                //}
                axes {
                    axis { 
                        name "system"
                        //values "gfs", "gefs"
                        values "gefs"
                    }
                }
                stages {
                    stage("build system") {
                        steps {
                            script {
                                def HOMEgfs = "${HOME}/${system}"
                                properties([parameters([[$class: 'NodeParameterDefinition', allowedSlaves: ['built-in','Hera-EMC','Orion-EMC'], defaultSlaves: ['built-in'], name: '', nodeEligibility: [$class: 'AllNodeEligibility'], triggerIfResult: 'allCases']])])
                                sh( script: "mkdir -p ${HOMEgfs}", returnStatus: true)
                                dir(HOMEgfs) {
                                    checkout scm
                                    env.MACHINE_ID = MACHINE
                                    if (fileExists("sorc/BUILT_semaphor")) {
                                        sh( script: "cat sorc/BUILT_semaphor", returnStdout: true).trim()
                                        pullRequest.comment("Cloned PR already built (or build skipped) on ${machine} in directory ${HOMEgfs}")
                                    } else {
                                        sh( script: "git submodule update --init --recursive", returnStatus: true) 
                                        def builds_file = readYaml file: "ci/cases/yamls/build.yaml"
                                        if (system == "gfs") {
                                            def system_key = string( system )
                                            def build_args = builds_file.find { it.system_key == system_key }.build_args
                                            echo "build args: ${build_args}"
                                            dir("${HOMEgfs}/sorc") {
                                                sh( script: "./${build_args}", returnStatus: false)
                                                sh( script: "./link_workflow.sh", returnStatus: false)
                                                sh( script: "echo ${HOMEgfs} > BUILT_semaphor", returnStatus: true)
                                            }
                                        } else if (system == "gefs") {
                                            // def system_key = string( system )
                                            def system_key = "gefs"
                                            def build_args = builds_file.find { it.system_key == system_key }.build_args
                                            echo "build args: ${build_args}"
                                            dir("${HOMEgfs}/sorc") {
                                                sh( script: "./build_all.sh -gu -j 4", returnStatus: false)
                                                sh( script: "./link_workflow.sh", returnStatus: false)
                                                sh( script: "echo ${HOMEgfs} > BUILT_semaphor", returnStatus: true)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }


        stage('Build GFS') {
            matrix {
                agent { label "${MACHINE}-emc" }
                //options {
                //    throttle(['Matrix_build'])
                //}
                axes {
                    axis { 
                        name "system"
                        //values "gfs", "gefs"
                        values "gfs"
                    }
                }
                stages {
                    stage("build system") {
                        steps {
                            script {
                                def HOMEgfs = "${HOME}/${system}"
                                properties([parameters([[$class: 'NodeParameterDefinition', allowedSlaves: ['built-in','Hera-EMC','Orion-EMC'], defaultSlaves: ['built-in'], name: '', nodeEligibility: [$class: 'AllNodeEligibility'], triggerIfResult: 'allCases']])])
                                sh( script: "mkdir -p ${HOMEgfs}", returnStatus: true)
                                dir(HOMEgfs) {
                                    checkout scm
                                    env.MACHINE_ID = MACHINE
                                    if (fileExists("sorc/BUILT_semaphor")) {
                                        sh( script: "cat sorc/BUILT_semaphor", returnStdout: true).trim()
                                        pullRequest.comment("Cloned PR already built (or build skipped) on ${machine} in directory ${HOMEgfs}")
                                    } else {
                                        sh( script: "git submodule update --init --recursive", returnStatus: true) 
                                        def builds_file = readYaml file: "ci/cases/yamls/build.yaml"
                                        if (system == "gfs") {
                                            def build_args = builds_file.find { it.system == system }.build_args
                                            echo "build args: ${build_args}"
                                            dir("${HOMEgfs}/sorc") {
                                                sh( script: "./${build_args}", returnStatus: false)
                                                sh( script: "./link_workflow.sh", returnStatus: false)
                                                sh( script: "echo ${HOMEgfs} > BUILT_semaphor", returnStatus: true)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        stage('Setup RUNTESTS') {
            agent { label "${MACHINE}-emc" }
            steps {
                script {
                    sh( script: "mkdir -p ${HOME}/RUNTESTS", returnStatus: true)
                    if ( pullRequest.labels.any{ value -> value.matches("CI-${machine}-Building") } ) {
                         pullRequest.removeLabel("CI-${machine}-Building")
                    }
                    pullRequest.addLabel("CI-${machine}-Running")
                }
            }

        }

        stage('Run Tests') {
            matrix {
                agent { label "${MACHINE}-emc" }
                axes {
                    axis {
                        name "Case"
                        values "C48_ATM", "C48_S2SWA_gefs", "C48_S2SW", "C96_atm3DVar"
                    }
                }
                stages {
                    stage('Create Experiment') {
                        steps {
                                script {
                                    def yaml_case = readYaml file: "${HOME}/gfs/ci/cases/pr/${Case}.yaml"
                                    system = yaml_case.experiment.system
                                    def HOMEgfs = "${HOME}/${system}"
                                    env.RUNTESTS = "${HOME}/RUNTESTS"
                                    sh( script: "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh create_experiment ${HOMEgfs}/ci/cases/pr/${Case}.yaml", returnStatus: true)
                                }
                        }
                    }
                    stage('Run Experiments') {
                        steps {
                            script {
                                def HOMEgfs = "${HOME}/gfs"
                                pslot = sh( script: "${HOMEgfs}/ci/scripts/utils/ci_utils_wrapper.sh get_pslot ${HOME}/RUNTESTS ${Case}", returnStdout: true ).trim()
                                pullRequest.comment("Running experiments: ${Case} with pslot ${pslot} on ${machine}")
                            try {
                                sh( script: "${HOMEgfs}/ci/scripts/run-check_ci.sh ${HOME} ${pslot}", returnStatus: false)
                                pullRequest.comment("SUCCESS running experiments: ${Case} on ${machine}")
                            } catch (Exception e) {
                                pullRequest.comment("FAILURE running experiments: ${Case} on ${machine}")
                                error("Failed to run experiments ${Case} on ${machine}")
                                }
                            }
                        }
                    }
                }
            }
        }

    }

    post {
        always {
            script {
                for (label in pullRequest.labels) {
                    if (label.contains("${machine}")) {
                        pullRequest.removeLabel(label)
                    }
                }
            }
        }
        success {
            script {
                pullRequest.addLabel("CI-${machine}-Passed")
                def timestamp = new Date().format("MM dd HH:mm:ss", TimeZone.getTimeZone('America/New_York'))
                pullRequest.comment("SUCCESSFULLY ran all CI Cases on ${machine} at ${timestamp}")
            }
        }
        failure {
            script {
                pullRequest.addLabel("CI-${machine}-Failed")
                def timestamp = new Date().format("MM dd HH:mm:ss", TimeZone.getTimeZone('America/New_York'))
                pullRequest.comment("CI FAILED ${machine} at ${timestamp}\n\nBuilt and ran in directory ${HOME}")
                if (fileExists('${HOME}/RUNTESTS/ci.log')) {
                    def fileContent = readFile '${HOME}/RUNTESTS/ci.log'
                        fileContent.eachLine { line ->
                        if( line.contains(".log")) {
                            archiveArtifacts artifacts: "${line}", fingerprint: true
                        }
                    }
                }
            }
        }
    }

}
