import * as core from '@actions/core';
import * as exec from "@actions/exec";
import * as io from "@actions/io";

async function run(): Promise<void> {
    try {
        const version = core.getInput("version");
        const emacsCIVersion = "emacs-" + version.replace(".", "-");

        core.startGroup("Installing Emacs");

        core.endGroup();

    } catch (error) {
        core.setFailed(error.message)
    }
}

run();
