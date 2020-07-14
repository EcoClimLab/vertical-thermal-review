% Kristina Anderson-Teixeira
% June 2020

%% INITIATE
clear all; clf; clc; clear; close all;

%% READ IN DATA & pull out variables
table=readtable('correlation_with_CRU_SCBI_1901_2016_climate_data.csv');
column_names=table.Properties.VariableNames;
%table.sp%table.month
%table.variable
%table.CP
n_records=height(table);
species=unique(table.sp);
n_sp=length(species);
months = {'curr.may' 'curr.jun' 'curr.jul' 'curr.aug'};
n_mo=length(months);
variables= {'tmp' 'tmn' 'tmx'};
n_var=length(variables);

%% PULL OUT RECORDS OF INTEREST

%create matrices to store rearranged values (rows are species, in alphabetical order; columns months in order)
tmp_und=NaN*ones(n_sp, n_mo);
tmp_can=NaN*ones(n_sp, n_mo);
tmn_und=NaN*ones(n_sp, n_mo);
tmn_can=NaN*ones(n_sp, n_mo);
tmx_und=NaN*ones(n_sp, n_mo);
tmx_can=NaN*ones(n_sp, n_mo);


for s=1:n_sp% cycle through species
    for m= 1:n_mo % cycle through months 
        
        %tmp-und
        ind=find (strcmp(char(string(species(s))),cellstr(table.sp))==1 & ...
                   strcmp('subcanopy',cellstr(table.CP))==1 &...
                   strcmp('tmp',cellstr(table.variable))==1 & ...
                    strcmp(char(string(months(m))), cellstr(table.month))==1);
        if isempty(ind)==0
            tmp_und(s,m)= table.coef(ind);
        end
         
        
        %tmp-can
        ind=find (strcmp(char(string(species(s))),cellstr(table.sp))==1 & ...
                   strcmp('canopy',cellstr(table.CP))==1 &...
                   strcmp('tmp',cellstr(table.variable))==1 & ...
                    strcmp(char(string(months(m))), cellstr(table.month))==1);
        if isempty(ind)==0
            tmp_can(s,m)= table.coef(ind);
        end
        
        %tmn-und
        ind=find (strcmp(char(string(species(s))),cellstr(table.sp))==1 & ...
                   strcmp('subcanopy',cellstr(table.CP))==1 &...
                   strcmp('tmn',cellstr(table.variable))==1 & ...
                    strcmp(char(string(months(m))), cellstr(table.month))==1);
        if isempty(ind)==0
            tmn_und(s,m)= table.coef(ind);
        end
         
        
        %tmn-can
        ind=find (strcmp(char(string(species(s))),cellstr(table.sp))==1 & ...
                   strcmp('canopy',cellstr(table.CP))==1 &...
                   strcmp('tmn',cellstr(table.variable))==1 & ...
                    strcmp(char(string(months(m))), cellstr(table.month))==1);
        if isempty(ind)==0
            tmn_can(s,m)= table.coef(ind);
        end
            
        %tmx-und
        ind=find (strcmp(char(string(species(s))),cellstr(table.sp))==1 & ...
                   strcmp('subcanopy',cellstr(table.CP))==1 &...
                   strcmp('tmx',cellstr(table.variable))==1 & ...
                    strcmp(char(string(months(m))), cellstr(table.month))==1);
        if isempty(ind)==0
            tmx_und(s,m)= table.coef(ind);
        end
         
        
        %tmx-can
        ind=find (strcmp(char(string(species(s))),cellstr(table.sp))==1 & ...
                   strcmp('canopy',cellstr(table.CP))==1 &...
                   strcmp('tmx',cellstr(table.variable))==1 & ...
                    strcmp(char(string(months(m))), cellstr(table.month))==1);
        if isempty(ind)==0
            tmx_can(s,m)= table.coef(ind);
        end
        

    end
end

%% MAKE PLOTS
sp_plot= [1:6 8:9 11:14];

figure (1)
plot(tmp_can(sp_plot,1:4),tmp_und(sp_plot,1:4),'o'); 
legend(months); hold on;
refline(1,0)
title('tmp')
xlabel('canopy')
ylabel('understory')

figure (2)
plot(tmn_can(sp_plot,1),tmn_und(sp_plot,1),'ob');  hold on;
plot(tmp_can(sp_plot,1),tmp_und(sp_plot,1),'sk');  hold on;
plot(tmx_can(sp_plot,1),tmx_und(sp_plot,1),'vr');  hold on;
refline(1,0);
legend('T_{min}', 'T_{mean}', 'T_{max}', '1:1','Location', 'best');
legend('boxoff');
title('Correlation of growth index with May temperatures')
xlabel('canopy trees')
ylabel('understory trees')



figure (4)
st_n=[5];
si_n=[1:4 6 8:9 11:14]; 
plot(tmp_can(st_n,1),tmp_und(st_n,1),'sk', 'MarkerSize', 14,'HandleVisibility','off'); hold on;
plot(tmp_can(si_n,1),tmp_und(si_n,1),'sk', 'MarkerSize', 14,'HandleVisibility','off'); hold on;
refline(1,0); hold on;
yline(0,'--k'); hold on;
ax = gca;
ax.FontSize = 22; 
legend('1:1','Location', 'northeast', 'FontSize', 20);
legend('boxoff');
%legend('shade intolerant species', 'shade tolerant', '1:1','Location', 'best');
title('Correlation of growth index with mean May T')
xlabel('T corr. of canopy tree growth', 'FontSize', 20)
ylabel('T corr. of understory tree growth','FontSize', 20)
print('-f4', 'SCBIfig', '-dpng','-r700')
shg
