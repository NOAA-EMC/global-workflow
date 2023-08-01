help([[
Load environment to run GFS workflow setup scripts on Jet
]])

load(pathJoin("rocoto", "1.3.3"))

if (mode() == "unload") then
    -- `execute` delays commands until last, but we need conda deactivated
    --   before unloading miniconda. `print` (bizarrely) still executes the
    --   command, but does it immediately. The semicolon is necessary 
    --   because otherwise other commands get tacked onto the same line.
    print("conda deactivate;")
end

-- Temporary until official hpc-stack is updated
prepend_path("MODULEPATH", "/mnt/lfs4/HFIP/hfv3gfs/role.epic/miniconda3/modulefiles")
load(pathJoin("miniconda3", "4.12.0"))
if (mode() == "load") then
    execute{cmd="conda activate ufswm", modeA={"load"}}
end

whatis("Description: GFS run setup environment")
