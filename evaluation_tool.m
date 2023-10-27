classdef app1_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                matlab.ui.Figure
        CPMEditField            matlab.ui.control.NumericEditField
        CPMEditFieldLabel       matlab.ui.control.Label
        FBPMEditField           matlab.ui.control.NumericEditField
        FBPMEditFieldLabel      matlab.ui.control.Label
        classesNEditField       matlab.ui.control.NumericEditField
        classesNEditFieldLabel  matlab.ui.control.Label
        simEditField            matlab.ui.control.NumericEditField
        simEditFieldLabel       matlab.ui.control.Label
        obsEditField            matlab.ui.control.NumericEditField
        obsEditFieldLabel       matlab.ui.control.Label
        CalculateButton         matlab.ui.control.Button
        MethodsDropDown         matlab.ui.control.DropDown
        MethodsDropDownLabel    matlab.ui.control.Label
        Panel                   matlab.ui.container.Panel
        Lamp                    matlab.ui.control.Lamp
        uploaddataxlsxButton    matlab.ui.control.Button
    end

    
    properties (Access = private)
        data1; % excel data input
        mthname % method name which one is selected
    end
    
    methods (Access = private)
        
        function results = myfunc(app, observed, simulated, nclass);
        % m number of rows in vector
        
        
        m=length(observed);
        n=nclass;
        A=observed;
        B=simulated;
        
        p=app.MethodsDropDown.Value;
        if strcmp(p,'Equal_Interval')
        CI=equalinterval(app, A, nclass);         
        elseif strcmp(p, 'Percentile_Based')
               CI=Percentile_Based(app, A, nclass);
        elseif strcmp(p, 'K_Means_Clustering')
            CI=K_Means_Clustering(app, A, nclass);
        end

        %calculation of C
        zx=m, zy=1;
        ax=m;
    for yo=1:ax
    if yo==1
        Cx(yo,1)=0;
    elseif yo>1
        Cx(yo,1)=A(yo-1,1); %one lagged flow
    end
   end
   C=Cx;

        D=zeros(m,1); E=zeros(m,1); CD=zeros(m,1);                                                                                                                                                          
        for j=1:n-1
            ix=find(A(:,1)>=CI(j) & A(:,1)<CI(j+1));
            ixx=find(B(:,1)>=CI(j) & B(:,1)<CI(j+1));
            ixz=find(C(:,1)>=CI(j) & C(:,1)<CI(j+1));
            D(ix,1)=j;
            if isempty(ixz)
                CD(ixz,1)=0;
            else
                CD(ixz,1)=j;
            end
            if isempty (ixx)
                E(ixx,1)=0;
            else
                E(ixx,1)=j;
            end
        end
        % time matching within each class interval
        % D and E contains all class interval assigned to given data
        for tk=1:nclass
            % clear jy G jz M1 N1 Cm1 Cm2 O1 Cm3 jzz GG
            jy=find(D(:,1)==tk);
            G=E(jy,1);
            GG=CD(jy,1);
            mx=length(jy); % total observed instances
            jz=find(G==tk);
            % % %             jzz=find(GG==tk);
            my=length(jz); % correctly classified instances
            Cm1=A(jy,1); % observed
            Cm2=B(jy,1); % predicted
            Cm3=C(jy,1); % naive model
            M1=Cm1(jz,1); % Time matching time series A - obs
            N1=Cm2(jz,1); % Time matching time series B - pred
            O1=Cm1(jz,1); % Time matching the naive model - obs
            O2=Cm3(jz,1); % Time matching the naive model - pred
            mz=mx-my; % incorrectly classified instances
            %             ix1=find(M1(:,1)==0); % if the observed dataset contains zero
            % FBPM
            if mx==0
                prob = 0;
            else
                prob=my/mx;
            end
            % variance ratio - time matching
            if isempty(M1) || isempty(N1)
                VRT=0;
            elseif var(M1)==0
                VRT=0;
            else
                VRT=var(N1)/var(M1); % pred/obs
            end
            % inter-quartile range ratio - time matching
            if isempty(M1) || isempty(N1)
                IQRT=0;
            elseif iqr(M1)==0
                IQRT=0;
            else
                IQRT=iqr(N1)/iqr(M1);
            end
            % correlation coefficient - time matching
            if isempty(M1) || isempty(N1)
                R = 0;
            elseif length(M1)<5
                R=0;
            else
                [R,RP]=corr(M1,N1,'Type','Pearson'); R;
            end
            % Bounded Relative Absolute Error (BRAE)
            clear BR
            if isempty(M1) || isempty(N1)
                BR=0;
            else
                for pt=1:length(M1)
                    et(pt)=abs(M1(pt)-N1(pt));
                    est(pt)=abs(O1(pt)-O2(pt));
                    if et(pt)==0 && est(pt)==0 % et and est both are zero, BR = 0.5
                        BR(pt,1)=0.5; % Chen et al. (2017)
                    else
                        BR(pt,1)=et(pt)/(et(pt)+est(pt));
                    end
                end
            end
            BRAE=sum(BR)/length(BR);

            % ks test - time matching
            if isempty(M1) || isempty(N1) || length(N1) < 3 || length(M1) < 3
                ks2statT = 1;  % empty set is not worth, that's why rejected H0
            else
                [hrT,prT,ks2statT]=kstest2(M1,N1);
            end
            QT(tk,:)=[tk mx my mz prob VRT IQRT R BRAE ks2statT];
        end
        %         col_ht={'Class','Total obs','Corr pred','Incorr pred','FBPM','VI','IQRI','CI','BRAEI','KS'};
        %         xlswrite(Filename,col_ht,Method{zz},xlRange1)
        %         xlswrite(Filename,QT,Method{zz},xlRange2)
        % Normalization of all indices between 0 and 1
        PT=QT(:,5:end);
        [ox,px]=size(PT);
        for i=1:nclass
            % FBPM
            if QT(i,2)==0 || QT(i,3)==0
                RT(i,1)=0;
            else
                RT(i,1)=PT(i,1);
            end
            % VI
            if QT(i,2)==0 || QT(i,3)==0
                RT(i,2)=0;
            elseif PT(i,2)<=1 && PT(i,2)>0
                RT(i,2)=PT(i,2);
            elseif PT(i,2)>1
                RT(i,2)=abs(1-(PT(i,2)-1)/PT(i,2));
            else
                RT(i,2)=0;
            end
            % IQRI
            if QT(i,2)==0 || QT(i,3)==0
                RT(i,3)=0;
            elseif PT(i,3)<=1 && PT(i,3)>0
                RT(i,3)=PT(i,3);
            elseif PT(i,3)>1
                RT(i,3)=abs(1-(PT(i,3)-1)/PT(i,3));
            else
                RT(i,3)=0;
            end
            % CI
            if QT(i,2)==0 || QT(i,3)==0
                RT(i,4)=0;
            elseif isnan(PT(i,4)) || PT(i,4)==Inf || PT(i,4)==-Inf
                RT(i,4)=0;
            else
                RT(i,4)=0.5*PT(i,4)+0.5;
            end
            % BRAE
            if QT(i,2)==0  || QT(i,3)==0
                RT(i,5)=0;
            else
                RT(i,5)=1-PT(i,5);
            end
            % KS
            if QT(i,2)==0  || QT(i,3)==0
                RT(i,6)=0;
            else
                RT(i,6)=1-PT(i,6);
            end
        end
        % penalized by multiplying FBPM to each index in each class
        ST=RT(:,2:end).*RT(:,1);
        SST=[RT(:,1) ST]; % FBPM is not to be added for computation of CPM
        % weights for different classifications
        %         if nclass==5
        %             EQW=(1/5)*ones(5,1);
        %             LFW=[0.6 0.1 0.1 0.1 0.1]';
        %             MFW=[0.134 0.3 0.3 0.133 0.133]';
        %             HFW=[0.134 0.133 0.133 0.3 0.3]';
        %         elseif nclass==10
        %             EQW=(1/10)*ones(10,1);
        %             LFW=[0.2 0.2 0.2 0.1 0.05 0.05 0.05 0.05 0.05 0.05]';
        %             MFW=[0.05 0.05 0.1 0.15 0.15 0.15 0.15 0.1 0.05 0.05]';
        %             HFW=[0.05 0.05 0.05 0.05 0.05 0.05 0.1 0.2 0.2 0.2]';
        %         elseif nclass==15
        %             EQW=(1/15)*ones(15,1);
        %             LFW=[(0.6/4)*ones(4,1);(0.4/11)*ones(11,1)];
        %             MFW=[(0.4/8)*ones(4,1);(0.6/7)*ones(7,1);(0.4/8)*ones(4,1)];
        %             HFW=[(0.4/11)*ones(11,1);(0.6/4)*ones(4,1)];
        %         elseif nclass==20
        %             EQW=(1/20)*ones(20,1);
        %             LFW=[(0.6/6)*ones(6,1);(0.4/14)*ones(14,1)];
        %             MFW=[(0.4/12)*ones(6,1);(0.6/8)*ones(8,1);(0.4/12)*ones(6,1)];
        %             HFW=[(0.4/14)*ones(14,1);(0.6/6)*ones(6,1)];
        %         end
        EQW=(1/nclass)*ones(nclass,1); %EQW means equal weights
        ab1=sum(EQW);
        FBPM=sum(RT(:,1).*EQW); %FBPM
        %         ab2=sum(LFW); ab3=sum(MFW); ab4=sum(HFW);
        if round(ab1,0)~=1 %|| round(ab2,0)~=1 || round(ab3,0)~=1 || round(ab4,0)~=1
            disp('ERROR in weights')
        else
            disp('Weights sum up to one')
        end
        % final matrix multiplied by class interval weights
        %for jx=1:px
            %             NS1(:,jx)=sum(RT(:,jx).*EQW); % RT is non-penalized matrix
            %             NS2(:,jx)=sum(RT(:,jx).*LFW);
            %             NS3(:,jx)=sum(RT(:,jx).*MFW);
            %             NS4(:,jx)=sum(RT(:,jx).*HFW);
        %end

        % Final CPM considering all classes
        for jv=1:px-1
            SS1(:,jv)=sum(ST(:,jv).*EQW); % ST is penalized matrix
            %             SS2(:,jv)=sum(ST(:,jv).*LFW);
            %             SS3(:,jv)=sum(ST(:,jv).*MFW);
            %             SS4(:,jv)=sum(ST(:,jv).*HFW);
        end
        %         NPM=[NS1;NS2;NS3;NS4];
        %         SSM=[SS1;SS2;SS3;SS4];
        % normal composite score
        %         CS1=mean(NS1');
        %         CS2=mean(NS2');
        %         CS3=mean(NS3');
        %         CS4=mean(NS4');
        %         CPM=[CS1;CS2;CS3;CS4];
        % penalized composite score
        CPM=mean(SS1'); % CPM value
        results(1)=CPM;
        results(2)=FBPM;
         
        end
        
        function results = equalinterval(app, A, nclass)
                   a1=max(A);
                a2=min(A);
                CI=[];
       % CI = zeros(nclass, 1);
           c1=a1-a2;
          c2=c1/nclass;
       for i7=1:nclass
           Ci=a2+c2*i7;
           CI=[CI;Ci];
       end
       CI=[0;CI];
        results=CI;
        end
        
        function results = Percentile_Based(app, A, nclass)
         [f,xx]=ecdf(A);
         dt=(100/nclass)/100; % interval in decimals
         t=[0:dt:f(end)];
         yy=interp1(f,xx,t,'nearest');
         CI=yy';
         results=CI; 
        end
        
        function results = K_Means_Clustering(app, A, nclass)
     % Tc = nclass; %total no. of clusters
     % [idx,Co]=kmeans(A,Tc,'dist', 'sqEuclidean', 'start', 'cluster');
     % CI=sort(Co);
     % if CI(end)<xmax
     %    CI(end)=xmax;
      results=-1;
 
        end



    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Button pushed function: uploaddataxlsxButton
        function uploaddataxlsxButtonPushed(app, event)
                   [file, path] = uigetfile('*.xlsx', 'Select Excel File');
           
            if file ~= 0
                % Read the Excel file and store the data in the app's property
                % app.ExcelData = readtable(fullfile(path, file));
                app.data1= xlsread(fullfile(path, file));
                %update lamp to ensure that your data is uploded
                app.Lamp.Color='g';
                % now read two column
              % x=app.xEditField.Value;
              % y=app.y1EditField.Value;
          % column1 = app.data(:, x); % Extract the first column (assuming it exists)
          % column3 = app.data(:, y);
                 % app.UITable.Data=readtable(fullfile(path, file));
                % Display the file name
                % app.FileLabel.Text = ['File: ', file];
                % 
                % % Update the table data
                % app.DataTable.Data = app.ExcelData;
            end

        end

        % Clicked callback: MethodsDropDown
        function MethodsDropDownClicked(app, event)
            item = event.InteractionInformation.Item;
            
        end

        % Value changed function: MethodsDropDown
        function MethodsDropDownValueChanged(app, event)
            value = app.MethodsDropDown.Value;
            app.mthname=value;
        end

        % Button pushed function: CalculateButton
        function CalculateButtonPushed(app, event)
            %complete code to extract data according to provided column
            %number
              % column number for observed vector
               o=app.obsEditField.Value;

               % column number for simulated vector 
               s=app.simEditField.Value;
               % number of classes
               nclass=app.classesNEditField.Value;
               observed=app.data1(:, o);
               simulated=app.data1(:, s);
              res=myfunc(app, observed, simulated, nclass);
              app.CPMEditField.Value=res(1);
              app.FBPMEditField.Value=res(2);
            % y1=app.data(:, c2);x1=app.data(:, c1);
            % y1=app.data(:, c2);
               

        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 640 480];
            app.UIFigure.Name = 'MATLAB App';

            % Create Panel
            app.Panel = uipanel(app.UIFigure);
            app.Panel.Position = [12 407 219 43];

            % Create uploaddataxlsxButton
            app.uploaddataxlsxButton = uibutton(app.Panel, 'push');
            app.uploaddataxlsxButton.ButtonPushedFcn = createCallbackFcn(app, @uploaddataxlsxButtonPushed, true);
            app.uploaddataxlsxButton.Position = [18 10 110 23];
            app.uploaddataxlsxButton.Text = 'upload data(.xlsx)';

            % Create Lamp
            app.Lamp = uilamp(app.Panel);
            app.Lamp.Position = [157 11 20 20];
            app.Lamp.Color = [0.9412 0.9412 0.9412];

            % Create MethodsDropDownLabel
            app.MethodsDropDownLabel = uilabel(app.UIFigure);
            app.MethodsDropDownLabel.HorizontalAlignment = 'right';
            app.MethodsDropDownLabel.Position = [23 355 51 22];
            app.MethodsDropDownLabel.Text = 'Methods';

            % Create MethodsDropDown
            app.MethodsDropDown = uidropdown(app.UIFigure);
            app.MethodsDropDown.Items = {'Equal_Interval', 'Percentile_Based', 'K_Means_Clustering'};
            app.MethodsDropDown.ValueChangedFcn = createCallbackFcn(app, @MethodsDropDownValueChanged, true);
            app.MethodsDropDown.ClickedFcn = createCallbackFcn(app, @MethodsDropDownClicked, true);
            app.MethodsDropDown.Position = [89 355 100 22];
            app.MethodsDropDown.Value = 'Equal_Interval';

            % Create CalculateButton
            app.CalculateButton = uibutton(app.UIFigure, 'push');
            app.CalculateButton.ButtonPushedFcn = createCallbackFcn(app, @CalculateButtonPushed, true);
            app.CalculateButton.Position = [56 165 100 31];
            app.CalculateButton.Text = 'Calculate';

            % Create obsEditFieldLabel
            app.obsEditFieldLabel = uilabel(app.UIFigure);
            app.obsEditFieldLabel.HorizontalAlignment = 'right';
            app.obsEditFieldLabel.Position = [56 294 25 22];
            app.obsEditFieldLabel.Text = 'obs';

            % Create obsEditField
            app.obsEditField = uieditfield(app.UIFigure, 'numeric');
            app.obsEditField.Position = [96 294 43 22];

            % Create simEditFieldLabel
            app.simEditFieldLabel = uilabel(app.UIFigure);
            app.simEditFieldLabel.HorizontalAlignment = 'right';
            app.simEditFieldLabel.Position = [56 260 25 22];
            app.simEditFieldLabel.Text = 'sim';

            % Create simEditField
            app.simEditField = uieditfield(app.UIFigure, 'numeric');
            app.simEditField.Position = [96 260 44 22];

            % Create classesNEditFieldLabel
            app.classesNEditFieldLabel = uilabel(app.UIFigure);
            app.classesNEditFieldLabel.HorizontalAlignment = 'right';
            app.classesNEditFieldLabel.Position = [37 223 62 22];
            app.classesNEditFieldLabel.Text = 'classes(N)';

            % Create classesNEditField
            app.classesNEditField = uieditfield(app.UIFigure, 'numeric');
            app.classesNEditField.Position = [110 223 79 22];

            % Create FBPMEditFieldLabel
            app.FBPMEditFieldLabel = uilabel(app.UIFigure);
            app.FBPMEditFieldLabel.HorizontalAlignment = 'right';
            app.FBPMEditFieldLabel.Position = [48 115 38 22];
            app.FBPMEditFieldLabel.Text = 'FBPM';

            % Create FBPMEditField
            app.FBPMEditField = uieditfield(app.UIFigure, 'numeric');
            app.FBPMEditField.Position = [101 115 88 22];

            % Create CPMEditFieldLabel
            app.CPMEditFieldLabel = uilabel(app.UIFigure);
            app.CPMEditFieldLabel.HorizontalAlignment = 'right';
            app.CPMEditFieldLabel.Position = [48 79 32 22];
            app.CPMEditFieldLabel.Text = 'CPM';

            % Create CPMEditField
            app.CPMEditField = uieditfield(app.UIFigure, 'numeric');
            app.CPMEditField.Position = [95 79 94 22];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = app1_exported

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.UIFigure)

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.UIFigure)
        end
    end
end