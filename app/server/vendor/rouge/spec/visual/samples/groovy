// This source code comes from http://www.odelia-technologies.com/node/200

package com.odelia.groovy.simpleworkflow


class SimpleWorkflowEngine {
    def workflowMap = [:]
    def context = [:]
    def beforeActivityName = 'beforeActivity'
    def afterActivityName = 'afterActivity'

    SimpleWorkflowEngine(workflow, context = [:]) {
        this.context = context
        parseWorkflow(workflow)
    }

    def parseWorkflow(workflow) {
        workflowMap = new WorkflowParser().parse(workflow)
    }

    def getActivityValue(activity) {
        assert activity instanceof String
        if (!workflowMap[activity])
            throw new RuntimeException("$activity activity doesn't exist")
        workflowMap[activity]
    }

    def execute(activity, pause) {
        if (workflowMap[beforeActivityName]) {
            getActivityValue(beforeActivityName)(context, activity)
        }

        def activityValue = getActivityValue(activity)

        // Determine the next activity to execute
        def nextActivity
        switch (activityValue) {
            case String: nextActivity = activityValue; break
            case Closure: nextActivity = activityValue(context); break
            case Class: nextActivity = activityValue.newInstance()(context)
        }

        if (workflowMap[afterActivityName]) {
            getActivityValue(afterActivityName)(context, activity, nextActivity)
        }

        if (!pause && nextActivity)
            call(nextActivity)
        else
            nextActivity
    }

    def call(activity) {
        execute(activity, false)
    }

    def nextActivity(activity) {
        execute(activity, true)
    }

    static void main(String[] args) {
        if (args.size() != 2) {
            println 'Usage: com.odelia.groovy.simpleworkflow.SimpleWorkflowEngine <dsl_filename> <activity_name>'
            return
        }
        SimpleWorkflowEngine.newInstance(new File(args[0]))(args[1])
    }

}

private class WorkflowParser {
    def map = [:]

    def methodMissing(String name, args) {
        map[name] = args[0]
    }

    def parse(Closure wf) {
        wf.delegate = this
        wf.resolveStrategy = Closure.DELEGATE_FIRST
        wf()
        map
    }

    def workflow = { it ->
        it.delegate = this
        it.resolveStrategy = Closure.DELEGATE_FIRST
        it()
    }

    def parse(File workflowDef) {
        def binding = new Binding([workflow: workflow])
        def shell = new GroovyShell(binding)
        shell.evaluate(workflowDef)
        map
    }
}
