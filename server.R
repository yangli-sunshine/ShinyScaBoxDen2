library(ggplot2) 
library(ggpmisc)
library(ggpubr) 
library(gghalves) 
library(aplot)
library(showtext)

server <- function(input, output,session) {
  windowsFonts()
  ##定义好导入的字体
  Font <- c('STKaiti.TTF','simhei.TTF') ##华文楷体;黑体;
  for (i in Font) {
    font_path = i
    font_name = tools::file_path_sans_ext(basename(font_path))
    font_add(font_name, font_path)
  }
  font_families() ### 查看当前字体
  showtext_auto(enable=TRUE) #自动调用showtext，否则无法在ggsave()中使用，因为ggsave会自动打开和关闭图形设备。
  
  
  
    observeEvent(input$action,{
    df1 <- input$file$datapath
    df<-as.data.frame(read.table(df1,sep="\t",header=T,check.names=FALSE))
    sss6<-input$sss6
    if(sss6 == T){
      n1 <- "theme(panel.border = element_rect(color = input$xx, size = input$xxx))"}
    if(sss6==F){
      n1 <- "theme(panel.border = element_rect(color = NA, size = 0),axis.line = element_line(color = 'black'),
)
"}
    
   
    output$p4 <- renderPlot({
      #自定义颜色
      col<-c(input$c1)
      set.seed(42)
      
      
      ###绘图
      #散点图 
      min_x <- min(df$x)
      max_y <- max(df$y)
      
      # 计算 Pearson 相关系数和 p 值
      
      p1 <- ggplot(df,aes(x,y,fill=group))+
        geom_point(shape=as.numeric(input$sss5),size=input$sss4,alpha=input$sss3,color=input$c2,stroke = input$x)+
        geom_smooth(method = "lm",aes(color=group), se=T, 
                    formula = y ~ x,
                    linetype=1,alpha=0.5)+#线粗细   
        # 计算数据的最小值和最大值
       
      # 将最小值和最大值作为 label.x 和 label.y 的值传递给 stat_cor()
      stat_cor(method = "pearson", color = col,
               label.x = min_x, label.y = (max_y-1.5), size = 4)+#R方程
        stat_poly_eq(formula = y ~ x, 
                     aes(color=group,label = paste(after_stat(eq.label),
                                                   sep = "~~~")), parse = TRUE) +
        scale_fill_manual(values =input$color2)+#散点
        scale_color_manual(values = col)+#y与x式子
        theme_bw()+
        theme(panel.grid=element_blank(),
              axis.text.x = element_text(color=input$d1,size=input$ss1,angle=input$sss1),
              axis.text.y = element_text(color=input$d3,size=input$ss2,angle=input$sss2),
              axis.title.x = element_text(color=input$d2,size=input$ss3,family=input$ss5),
              axis.title.y = element_text(color=input$d4,size=input$ss4,family=input$ss6),
              
              legend.position = "none",
              
              
        )+
        eval(parse(text = n1))+
        labs(x=input$text1,y=input$text2)+
        
        scale_x_continuous(limits = c(min(df$x), max(df$x))) +
        scale_y_continuous(limits = c(min(df$y), max(df$y)))
      
      p1
      
      ###添加边际组合图形——散点+箱线图+半小提琴
      # 右边边际图
      p2 <- ggplot(df,aes(1,y))+
        geom_half_violin(fill=input$color4,position = position_nudge(x=0.26),side = "r",width=input$rr1,color=NA)+
        geom_boxplot(fill=input$color5,width=input$rr3,size=input$rr2,color=input$color11,position = position_nudge(x=0.2))+
        geom_jitter(fill=input$color6,shape=as.numeric(input$rr0),size=input$rr4,width=0.12,alpha=input$rr5,color=input$color13,stroke =input$rr6)+
        theme_void()+
        theme(legend.position = "none")
      p2
      #顶部边际图
      p3 <- ggplot(df,aes(1,x))+
        geom_half_violin(fill=input$color7,position = position_nudge(x=0.26),side = "r",width=input$tt1,color=NA)+
        geom_boxplot(fill=input$color8,width=input$tt3,size=input$tt2,color=input$color10,position = position_nudge(x=0.2))+
        geom_jitter(fill=input$color9,shape=as.numeric(input$tt0),size=input$tt4,width=0.12,alpha=input$tt5,color=input$color12,stroke = input$tt6)+
        theme_void()+
        theme(legend.position = "none")+
        coord_flip()
      p3
      #组合图形——基于aplot包进行组合
      
      p4 <<- p1%>%insert_top(p3,height = 0.4)%>%
        insert_right(p2,width = 0.4)
      p4
    })
    
    })
observe({
  ## *** Download PDF file ***
  output$downloadpdf <- downloadHandler(
    filename = function(){
      paste("plot.pdf")},
    content <- function(file){
      pdf(file,height = 750/72,width = 750/72)
      print(1)
      print(p4)
      dev.off()
    },contentType = "application/pdf"
  )
  
  
  
  ## *** Download SVG file ***
  output$downloadsvg <- downloadHandler(
    filename <- function(){ paste('plot.svg') },
    content <- function(file){
      svg(file,height = 750/72,width = 750/72)
      print(p4)
      dev.off()
    }, contentType = 'image/svg')
  
  
  output$Download <- downloadHandler(
    filename <- function() { paste('data.txt') },
    content <- function(file) {
      if ( exists("data.txt") ){
        print(TRUE)
      } else {
        exam1.fa <- readLines("data.txt")
      }
      writeLines(exam1.fa, con=file)
    }, contentType = 'text/plain')
})
}