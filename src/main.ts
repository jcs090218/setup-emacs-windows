import * as core from '@actions/core';
import * as exec from "@actions/exec";
import * as io from "@actions/io";
import * as tc from "@actions/tool-cache";

async function run(): Promise<void> {
    try {
        const version = core.getInput("version");
        const ver_lst = version.split(".");
        const emacs_major_ver = ver_lst[0];
        const emacs_minor_ver = ver_lst[1];
        const dot_ver = emacs_major_ver + "." + emacs_minor_ver;
        const dash_ver = emacs_major_ver + "-" + emacs_minor_ver;
        const emacs_dot_var = "emacs-" + dot_ver;
        const emacs_dash_ver = "emacs-" + dash_ver;

        core.startGroup("Installing Emacs");
        const ftpUrl = "https://ftp.gnu.org/gnu/emacs/windows/emacs-" + emacs_major_ver + "/";
        let zipPath = ftpUrl + emacs_dot_var;

        switch (dot_ver) {
            case "23.4":
            case "24.1":
            case "24.2":
            case "24.3": {
                zipPath += "-bin-i386.zip";
                break;
            }
            case "24.4": {
                zipPath += "-bin-i686-pc-mingw32.zip";
                break;
            }
            case "24.5": {
                zipPath += "-bin-i686-mingw32.zip";
                break;
            }
            case "25.1": {
                zipPath += "-x86_64-w64-mingw32.zip";
                break;
            }
            case "25.2":
            case "25.3":
            case "26.1":
            case "26.2":
            case "26.3":
            case "27.1": {
                zipPath += "-x86_64.zip";
                break;
            }
            default: {
                zipPath += "-x86_64.zip";
                break;
            }
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
