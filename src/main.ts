import * as core from '@actions/core';
import * as exec from "@actions/exec";
import * as io from "@actions/io";
import * as tc from "@actions/tool-cache";

async function run(): Promise<void> {
    try {
        const version = core.getInput("version");
        const emacs_dot_var = "emacs-" + version;
        const emacs_dash_ver = emacs_dot_var.replace(".", "-");

        core.startGroup("Installing Emacs");
        const ftpUrl = "https://ftp.gnu.org/gnu/emacs/windows/" + emacs_dash_ver + "/";
        const zipPath = ftpUrl + emacs_dot_var + "-x86_64.zip";
        const emacsZip = await tc.downloadTool(zipPath);
        const emacsDir = await tc.extractZip(emacsZip, './');

        const emacsBin = "%PATH%;" + emacsDir + "bin";
        console.log("emacsBin: " + emacsBin);
        core.exportVariable("PATH", emacsBin);

        core.endGroup();

    } catch (error) {
        core.setFailed(error.message);
    }
}

run();
