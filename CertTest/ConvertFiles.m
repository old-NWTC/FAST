%Conversion of FAST v 7.x files to FAST v8.0.0
% by Bonnie Jonkman
%  based on "Demonstration of fast file manipuation" by Paul Fleming
% (c) 2011, 2013 National Renewable Energy Laboratory
%--------------------------------------------------------------------------
clear all;

FASTSimulationToolbox = 'C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\branches\BJonkman\UtilityCodes\SimulationToolbox';
addpath( genpath( FASTSimulationToolbox ) );

% FstFileDir  = 'C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\trunk\CertTest\TstFiles';
% RootName    = 'Test11';
% inputfile  = [FstFileDir '\'          RootName '.fst'];
% outputfile = [FstFileDir '\TstFiles\' RootName '.out'];
% [YawManRat, PitManRat] = CalculateYawAndPitchRates(inputFile, outputFile);


%%
oldDir      = 'C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\trunk\CertTest';
newDir      = '.';
outFileDir  = 'C:\Users\bjonkman\Documents\DATA\DesignCodes\simulators\FAST\SVNdirectory\trunk\CertTest';
% templateDir = 'TemplateFiles\V8.00.x\5MW_Monopile';
% XLS_file  = '..\OutListParameters.xlsx';

%test 9:  YawManRat    = 3.729000
%test 11: PitManRat(1) = 16.600000

for i= 8:17 %1:(17+5) %17+(1:5) %1:17 %

        % Primary input file:
        
    baseFileName  = ['Test' num2str(i,'%02.0f') ];
 
    inputfile = [oldDir filesep baseFileName '.fst'];      

    if i==9
        outputfile = [outFileDir filesep baseFileName '.out'];
        [YawManRat, PitManRat] = CalculateYawAndPitchRates(inputfile, outputfile);
%         YawManRat = 3.729000;
        ConvertFAST7to8(inputfile,newDir,YawManRat);
    elseif i==11
        outputfile = [outFileDir filesep baseFileName '.out'];
        [YawManRat, PitManRat] = CalculateYawAndPitchRates(inputfile, outputfile);
%         PitManRat(1) = 16.6;
        ConvertFAST7to8(inputfile,newDir,0,PitManRat);
    else
        ConvertFAST7to8(inputfile,newDir);
    end 
                       
end
return;



