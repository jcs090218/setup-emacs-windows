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
        let zipPath = ftpUrl + emacs_dot_var;

        switch (emacs_dot_var) {
            case "23.4":
            case "24.1":
            case "24.2":
            case "24.3":
                zipPath += "-bin-i386.zip";
                break;
            case "24.4":
                zipPath += "-bin-i686-pc-mingw32.zip";
                break;
            case "24.5":
                zipPath += "-bin-i686-mingw32.zip";
                break;
            case "25.1":
                zipPath += "-x86_64-w64-mingw32.zip";
                break;
            case "25.2":
            case "25.3":
            case "26.1":
            case "26.2":
            case "26.3":
            case "27.1":
                zipPath += "-x86_64.zip";
                break;
            default:
                zipPath += "-x86_64.zip";
                break;
        }

        console.log("zipPath: " + zipPath);
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
