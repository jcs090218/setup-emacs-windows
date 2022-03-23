import * as core from '@actions/core';
import * as exec from "@actions/exec";
import * as io from "@actions/io";
import * as tc from "@actions/tool-cache";
import fs from 'fs'

async function run(): Promise<void> {
    try {
        const PATH = process.env.PATH;

        const version = core.getInput("version");
        const ver_lst = version.split(".");  // if 27.1
        const emacs_major_ver = ver_lst[0];  // 27
        const emacs_minor_ver = ver_lst[1];  // 1
        const dot_ver = emacs_major_ver + "." + emacs_minor_ver;  // 27.1
        const dash_ver = emacs_major_ver + "-" + emacs_minor_ver;  // 27-1
        const emacs_dot_var = "emacs-" + dot_ver;  // emacs-27.1

        core.startGroup("Installing Emacs");
        let ftpUrl = "https://ftp.gnu.org/gnu/emacs/windows/emacs-" + emacs_major_ver + "/";
        let zipPath = ftpUrl + emacs_dot_var;

        if (version == "snapshot") {
            // NOTE: If snapshot, directly assign the newest version.
            // Current newest snaptshot is `29.0.50`.
            zipPath = "https://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-29/emacs-29.0.50-snapshot-2022-03-19.zip";
        } else {
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
                case "27.1":
                case "27.2":{
                    zipPath += "-x86_64.zip";
                    break;
                }
                default: {
                    zipPath += "-x86_64.zip";
                    break;
                }
            }
        }

        const extractPath = "c:\\emacs";
        if (!fs.existsSync(extractPath)) {
            fs.mkdirSync(extractPath);
        }

        const emacsZip = await tc.downloadTool(zipPath);
        const emacsDir = await tc.extractZip(emacsZip, extractPath);

        let emacsRoot = emacsDir;
        let emacsBin = emacsRoot + "\\bin";
        if (!fs.existsSync(emacsBin)) {
            emacsRoot = emacsDir + "\\" + emacs_dot_var;
            emacsBin = emacsRoot + "\\bin";  // Refresh
        }

        core.exportVariable("PATH", `${PATH};${emacsRoot}`);
        core.exportVariable("PATH", `${PATH};${emacsBin}`);

        core.endGroup();

    } catch (error) {
        core.setFailed(error.message);
    }
}

run();
