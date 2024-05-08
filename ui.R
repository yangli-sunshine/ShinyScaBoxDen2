library(ggplot2) 
library(ggpmisc)
library(ggpubr) 
library(gghalves)
library(colourpicker)
library(shinyBS)
shinyUI(navbarPage(
  "Scatter+Curve fitting+Marginal Plot",
  tabPanel("散点图配合拟合曲线和边际分布图",
           sidebarPanel(
             
             fileInput("file", label="选择文件", accept = ".txt"),
             downloadButton("Download", "Download example data"),
             br(),
             br(),
             h3('图形参数'),tags$style(HTML("
    h4 {
      color: #F77423
    ;
    }
  ")),
             
             
             selectInput("select",label = h4("图形种类"), 
                         choices = list("散点图","右部边际图","顶部边际图")), 
             conditionalPanel(condition="input.select == '散点图'",
                              
                         checkboxInput("title", "添加标题", FALSE) ,
                              conditionalPanel(condition = "input.title",
                                textInput("text1", "横坐标标题:", value = c("")),
                                textInput("text2", "纵坐标标题:", value = c(""))),
                              
                              
                         checkboxInput("colour", "颜色", FALSE) ,
                              conditionalPanel(condition = "input.colour",
                                colourInput("color2",label = "散点和置信区间颜色",value="#EDBB47"),
                                colourInput("c2",label = "散点外圈颜色",value="black"),
                                colourInput("c1",label = "直线回归方程和回归直线颜色",value="#F77423")
                              ),
                              
                         
                         checkboxInput("others", "散点", FALSE) ,  
                               conditionalPanel(condition = "input.others",
                                sliderInput("sss3",label = "点的透明度",min = 0,max = 1,value = 0.5),
                                sliderInput("sss4",label = "点的大小",min = 1,max = 10,value = 3),
                                sliderInput("x",label = "点外圈的粗细",min = 0,max = 2.4,value = 0.8),
                                selectInput("sss5",label = "点的形状",choices = list("圆形"=21,"正方形"=22,"菱形"=23,"正三角形"=24,"倒三角形"=25)),
                                 ),
                              
                              
                              
                        checkboxInput("zbz", "坐标", FALSE) ,
                          conditionalPanel( condition = "input.zbz",
                            fluidRow(column(6, checkboxInput("showXAxis", "横坐标轴", FALSE),
                                 conditionalPanel(condition = "input.showXAxis",
                                   sliderInput("ss1", label = "横坐标字体大小", min = 5, max = 30, value = 20),
                                   colourInput("d1",label = "横坐标字体颜色",value="black"),
                                   
                                   sliderInput("ss3", label = "横标题字体大小", min = 5, max = 55, value = 20),
                                   colourInput("d2",label = "横标题字体颜色",value="black"),
                                   selectInput("ss5", label = "横标题字体样式", choices = list("sans", "serif", "mono", "wqy-microhei" ,"STKaiti", "simhei" ), selected = "sans"),
                                   sliderInput("sss1",label = "横坐标倾斜度",min = -90,max = 90,value = 0),
                                       )),
                                 column(6, checkboxInput("showYAxis", "纵坐标轴", FALSE),
                                 conditionalPanel(condition = "input.showYAxis",
                                   sliderInput("ss2", label = "纵坐标字体大小", min = 5, max = 30, value = 20),
                                   colourInput("d3",label = "纵坐标字体颜色",value="black"),
                                   sliderInput("ss4", label = "纵标题字体大小", min = 5, max = 55, value = 20),
                                   colourInput("d4",label = "纵标题字体颜色",value="black"),
                                   selectInput("ss6", label = "纵标题字体样式", choices = list("sans", "serif", "mono", "wqy-microhei" ,"STKaiti", "simhei" ), selected = "sans"),
                                   sliderInput("sss2",label = "纵坐标倾斜度",min = 0,max = 180,value = 90),
                                       )))),
                        checkboxInput("sss", label = "边框"),
                        conditionalPanel(condition = "input.sss",
                                         checkboxInput("sss6", label = "是否显示边框"),
                                         colourInput("xx",label = "边框颜色",
                                                     value="#EDBB47"),
                                         sliderInput("xxx",label = "边框粗细",min = 0.5,max = 3,value = 0.8)),
                        
                        ),
             
             
             
             
         
          
             conditionalPanel(condition="input.select == '右部边际图'",

                          checkboxInput("rc", "颜色", FALSE) ,
                              conditionalPanel(condition = "input.rc",       
                                colourInput("color4",label = "概率密度图颜色",value="#FFEAB5"),
                                colourInput("color5",label = "箱图颜色",value="#FFE570"),
                                colourInput("color11",label = "箱图边框颜色",value="black"),
                                colourInput("color6",label = "点颜色",value="#a899e5"),
                                colourInput("color13",label = "点外圈颜色",value="black")
                                ),
                          checkboxInput("conn", "箱图", FALSE) ,      
                          conditionalPanel(condition = "input.conn ",
                                           sliderInput("rr3",label = "箱图宽度",min = 0.05,max = 0.3,value = 0.1),
                                           sliderInput("rr2",label = "箱图边框粗细",min = 0.1,max = 2,value = 1.2)),
                          
                          checkboxInput("spott", "点", FALSE) ,      
                          conditionalPanel(condition = "input.spott ",      
                                           sliderInput("rr1",label = "点的分布",min = 0.1,max = 5,value = 0.5),
                                           
                                           sliderInput("rr4",label = "点的大小",min = 1,max = 6,value = 3),
                                           sliderInput("rr5",label = "点的透明度",min = 0.1,max = 1,value = 0.5),
                                           sliderInput("rr6",label = "点外圈粗细",min = 0,max = 1.6,value = 0.8),
                          selectInput("rr0",label = "点的形状",choices = list("圆形"=21,"正方形"=22,"菱形"=23,"正三角形"=24,"倒三角形"=25))),
                          
             ),
             conditionalPanel(condition="input.select == '顶部边际图'",
                             
                         checkboxInput("tc", "颜色", FALSE) ,
                              conditionalPanel(condition = "input.tc",    
                                colourInput("color7",label = "概率密度图颜色",value="#FFCBB5"),
                                colourInput("color8",label = "箱图颜色",value="#FF6E26"),
                                colourInput("color10",label = "箱图边框颜色", value="black"),
                                colourInput("color9",label = "点颜色",value="#FFB8DF"),
                                colourInput("color12",label = "点外圈颜色",value="black")),
                              
                         
                         checkboxInput("con", "箱图", FALSE) ,      
                             conditionalPanel(condition = "input.con ",
                                sliderInput("tt3",label = "箱图高度",min = 0.05,max = 0.3,value = 0.1),
                                sliderInput("tt2",label = "箱图边框粗细",min = 0.1,max = 2,value = 1.2)),
                         
                         checkboxInput("spot", "点", FALSE) ,      
                         conditionalPanel(condition = "input.spot ",      
                                sliderInput("tt1",label = "点的分布",min = 0.1,max = 5,value = 0.5),
                                
                                sliderInput("tt4",label = "点的大小",min = 1,max = 6,value = 3),
                                sliderInput("tt5",label = "点的透明度",min = 0.1,max = 1,value = 0.5),
                                sliderInput("tt6",label = "点外圈粗细",min = 0,max = 1.6,value = 0.8),
                                selectInput("tt0",label = "点的形状",choices = list("圆形"=21,"正方形"=22,"菱形"=23,"正三角形"=24,"倒三角形"=25)),
                         
                         )),
            

             br(),
                  
             # checkboxInput("downloadratio", h4("图片下载大小"), FALSE),
             # conditionalPanel(condition = "input.downloadratio",
            
             h4("图片下载"),
                              numericInput("lollipopHeight", "Plot download height", value="600"),
                              numericInput("lollipopWidth", "Plot download width", value="600"),
             # ) ,
             
             actionButton(
               "action",
               label = "go")) ,
           
           
           
           

           
           
           
           mainPanel(
             downloadButton("downloadpdf","Download pdf-file"),
             downloadButton("downloadsvg","Download svg-file"),
             
             plotOutput("p4",height="600px",width = "600px"),
             
           )),tabPanel("Help",includeMarkdown("README.md"))
  
  ))