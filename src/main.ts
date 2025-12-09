import * as core from '@actions/core';
import * as exec from "@actions/exec";
import * as tc from "@actions/tool-cache";
import fs from 'fs';

/**
 *  Return a Emacs zip path.
 */
function downloadEmacsZip(base: string, version: string) {
    const ver_lst = version.split(".");  // if 27.1
    const emacs_major_ver = ver_lst[0];  // 27
    const emacs_minor_ver = ver_lst[1];  // 1
    const dot_ver = emacs_major_ver + "." + emacs_minor_ver;  // 27.1
    const dash_ver = emacs_major_ver + "-" + emacs_minor_ver;  // 27-1
    const emacs_dot_var = "emacs-" + dot_ver;  // emacs-27.1

    let ftpUrl = base + "emacs-" + emacs_major_ver + "/";
    let zipPath = ftpUrl + emacs_dot_var;

    if (version == "snapshot") {
        // NOTE: If snapshot, directly assign the newest version.
        // Current newest snaptshot is `30.0.50`.
        zipPath = "https://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-30/emacs-30.1.90_1.zip";
    } else {
        switch (dot_ver) {
            case "22.3":
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
            case "27.2": {
                zipPath += "-x86_64.zip";
                break;
            }
            case "28.1":
            case "28.2":
            case "29.1":
            case "29.2":
            case "29.3":
            case "29.4":
            case "30.1":
            case "30.2": {
                zipPath += ".zip";
                break;
            }
            default: {
                zipPath += "-x86_64.zip";
                break;
            }
        }
    }

    return zipPath;
}

/**
 *  Attempt to download Emacs, else return null.
 */
async function downloadEmacs(base: string, version: string): Promise<string | null> {
    let zipPath = downloadEmacsZip(base, version);
    try {
        return tc.downloadTool(zipPath);
    } catch (err) {
        return null;
    }
}


/**
 *  Action entry.
 */
async function run(): Promise<void> {
    try {
        const PATH = process.env.PATH;

        const version = core.getInput("version");

        const emacsZip =
            await downloadEmacs("https://ftp.man.poznan.pl/gnu/emacs/windows/", version)
            ?? await downloadEmacs("https://ftp.gnu.org/gnu/emacs/windows/", version);

        if (!emacsZip) {
            throw new Error("Failed to download Emacs from all sources.");
        }

        // Setup extract path.
        const extractPath = "c:\\emacs";
        if (!fs.existsSync(extractPath)) {
            fs.mkdirSync(extractPath);
        }

        const emacsDir = await tc.extractZip(emacsZip, extractPath);

        let emacsRoot = emacsDir;
        let emacsBin = emacsRoot + "\\bin";

        if (!fs.existsSync(emacsBin)) {
            // It should only have one directory, which is the root directory.
            fs.readdirSync(emacsRoot).forEach(file => {
                emacsRoot = emacsDir + "\\" + file;
                emacsBin = emacsRoot + "\\bin";
            });
        }

        core.exportVariable("PATH", `${PATH};${emacsRoot}`);
        core.exportVariable("PATH", `${PATH};${emacsBin}`);

        core.endGroup();

        // show Emacs version
        await exec.exec('emacs', ['--version']);
    } catch (error) {
        if (error instanceof Error) {
            core.setFailed(error.message);
        } else {
            core.setFailed(String(error));
        }
    }
}

run();
