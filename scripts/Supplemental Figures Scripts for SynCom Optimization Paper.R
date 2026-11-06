#### Scripts for SynCom Optimization Paper ####
#### Supplemental Figures ####

####******************************************************####


#### Figure S1  ####



sampleColor <- colorRampPalette(c('#F6EB5E','#CD8280','#54D0B4','#78C3ED',
                                  '#E69F00','#3BFFB8','#ECAD67','#E98FBD',
                                  '#2672B2'))(n_materials)



# 清理R环境：移除所有对象+释放内存（确保脚本可重复运行）
rm(list = ls())
gc()

# 加载核心绘图包
library(tidyverse)   # 数据处理与基础绘图
library(ggridges)    # 山脊图专用包
library(patchwork)   # 多图排版

# 设置工作目录（需根据实际路径修改）
setwd("D:/work_year/4_WAZ")

# 读取数据（制表符分隔，含表头）
data <- read.table("./1_DRI/0_data.txt", header = T, sep = "\t")


# 查看数据结构（列类型、样本数等）
str(data) 
# 预览前6行（确认数据加载正确）
head(data)  

# 宽表转长表（ggplot2绘图标准格式）
data_long <- data %>%
  pivot_longer(
    cols = -Material,          # 保留Material列，其余列转为行
    names_to = "index",        # 原列名存入新列index
    values_to = "value"        # 数值存入新列value
  )

# 指定指标顺序（避免ggplot自动按字母排序）
colnames_data <- c("PH", "RL", "AFW", "BFW", "ADW", "BDW")
# 转换为因子并固定水平顺序
data_long$index <- factor(data_long$index, levels = colnames_data)


# 手动指定6个指标的颜色（HEX格式）
manual_colors <- c(
  'PH'  = '#CD8280',  # 黄色
  'RL'  = '#E98FBD',  # 粉红
  'AFW' = '#54D0B4',  # 蓝绿
  'BFW' = '#3BFFB8',  # 天蓝
  'ADW' = '#78C3ED',  # 橙色
  'BDW' = '#2672B2'   # 亮绿
)

RidgePlot1 <- ggplot(data_long, aes(x = value, y = index, fill = index)) +
  # 山脊图层：半透明密度曲线
  geom_density_ridges(alpha = 0.6, show.legend = FALSE) +
  
  # X轴配置
  scale_x_continuous(
    name = "DRI",                         # 轴标题
    limits = c(0, 3),                     # 固定坐标范围
    expand = expansion(mult = c(0, 0.05)),# 右侧留5%空白
    breaks = seq(0, 3, by = 0.5)          # 刻度间隔0.5
  ) +
  
  # Y轴配置
  scale_y_discrete(
    name = "Index",
    expand = expansion(add = c(0.1, 0.1)) # 上下扩展10%空间
  ) +
  
  # ⭐ 关键：注入自定义颜色（与因子顺序严格对应）
  scale_fill_manual(values = manual_colors) +
  
  # 标题设置
  labs(title = "Distribution by Index") +
  
  # 主题系统
  theme_minimal() +  # 简洁主题基底
  theme(
    # 坐标轴文字
    axis.text.x = element_text(size = 12, vjust = 0, margin = margin(t = 5)),
    axis.text.y = element_text(size = 12, margin = margin(r = -5)), # 右侧负边距节省空间
    # 坐标轴标题
    axis.title.x = element_text(size = 12, vjust = 0, margin = margin(t = 0)),
    axis.title.y = element_text(size = 12, vjust = 3, margin = margin(r = -5)), # 垂直偏移3pt
    # 主标题
    plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 10)), # 居中+下边距
    # 网格线
    panel.grid.major = element_line(color = "gray90", linewidth = 0.3), # 主网格浅灰
    panel.grid.minor = element_line(color = "gray95", linewidth = 0.2), # 次网格更浅
    # 整体边距（上右下左）
    plot.margin = margin(20, 20, 20, 20, unit = "pt")  
  )

# 显示图形
print(RidgePlot1)



# 保存图形
ggsave("1_DRI/RidgePlot1.png", RidgePlot1, width = 6, height = 4, dpi = 300)
# 保存图形为PDF
ggsave(
  filename = "1_DRI/RidgePlot_Distribution-1.pdf",  # 文件名
  plot = RidgePlot1,                       # 要保存的图形对象
  device = "pdf",                          # 输出格式
  width = 6,                              # 宽度（英寸）
  height = 4,                              # 高度（英寸）
  units = "in",                            # 单位
  dpi = 300                                # 分辨率（每英寸点数）
)



#### Figure S2 ####

rm(list = ls())
gc()
setwd("D:/work_year/4_WAZ")


if(!require(devtools)) install.packages("devtools")
# devtools::install_github("cardiomoon/moonBook")
# devtools::install_github("cardiomoon/webr")
require(ggplot2)
require(moonBook)
require(webr)
library(tidyr)     # 数据整理
library(dplyr)     # 数据处理
library(scales)    # 坐标轴格式化
library(grDevices) # 图形设备控制




##导入数据
df <- read.csv('./6_菌株信息饼图/pie_data.csv',row.names = 1)

df_summarized <- df %>%
  group_by(phylum, genus) %>%
  summarize(rate = sum(rate), .groups = "drop")
PieDonut(df_summarized, aes(phylum, genus, count = rate), ratioByGroup = FALSE,labelpositionThreshold=5)


PieDonut(df,                               
         aes(pies = phylum,               
             donuts = genus),              # 内环(环形图)变量: 可改为species等更细分类

         r0 = 0.15,                        # 内环内半径(中心空白): 推荐0.1-0.3
         r1 = 0.5,                         # 内环外半径: 通常设为r0+0.3-0.5
         r2 = 0.8,                         # 外环外半径: 通常比r1大0.2-0.4

         color = "gray",                   # 分隔线颜色: 可改为"white"/"black"或其他颜色
         pieAlpha = 1,                     # 外环透明度: 0(全透明)-1(不透明),推荐0.8-1
         donutAlpha = 0.8,                 # 内环透明度: 通常比外环低0.1-0.2
         
         # 起始角度 (弧度制)
         start = pi/2,                     # 起始角度: 

         addPieLabel = TRUE,               # 外环标签: TRUE/FALSE
         addDonutLabel = TRUE,             # 内环标签: TRUE/FALSE
         
         # 标签位置控制
         labelposition = 1,                # 标签位置: 
         # 0=外缘, 
         # 1=中部,
         # 2=内部(推荐)
         labelpositionThreshold = 0,    # 标签显示阈值(比例):

         showRatioDonut = TRUE,            # 显示内环比例: TRUE/FALSE
         showRatioPie = TRUE,             # 显示外环比例: TRUE/FALSE
         ratioByGroup = FALSE,             # 比例计算方式:
         # FALSE=全局比例(推荐),
         # TRUE=组内比例
         showRatioThreshold = 0.02,           # 比例标签显示阈值: 

         showPieName = TRUE,               # 显示外环类别名: TRUE/FALSE
         showDonutName = TRUE,            # 显示内环类别名: TRUE/FALSE
         title = "",                       # 主标题: 可设置如"微生物群落组成"
         pieLabelSize = 3,                 # 外环标签字号: 推荐3-5
         donutLabelSize = 3,               # 内环标签字号: 通常比外环小0.5-1
         titlesize = 3,                    # 标题字号: 推荐4-6

         family = "sans"                   # 无衬线字体
)



#### Figure S3 ####

rm(list = ls())
gc()


library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(GGally) # Extension to 'ggplot2'
library(ggh4x) # Hacks for 'ggplot2'

setwd("D:/work_year/4_WAZ")


df <- read.csv('./11_cor_syn/data.csv', row.names = 1)

# 定义分组颜色
# 创建包含两种颜色的向量，用于区分不同样本组
col <- c("#108B96", "#F3993A")  # 浅红色和浅青色

# 使用GGally包的ggpairs函数创建相关性矩阵图
cor <- ggpairs(
  df,  # 输入的数据框
  columns = 1:7,  # 只使用前7列数据（假设是农艺性状数据）
  
  # 设置美学映射（aesthetics）
  mapping = aes(color = samples, fill = samples),  # 根据samples列分组着色
  
  # 设置下三角区域（左下部分）的图形
  lower = list(
    # 连续变量：显示相关系数
    continuous = wrap("cor", size = 3.5),  
    # 组合变量：显示箱线图
    combo = wrap("box_no_facet", color = "black")  
  ),
  
  # 设置对角线区域（从左上到右下）的图形
  diag = list(
    # 连续变量：显示密度曲线
    continuous = wrap("densityDiag", color = "black", alpha = 0.8)  
  ),
  
  # 设置上三角区域（右上部分）的图形
  upper = list(
    # 连续变量：显示散点图和拟合曲线
    continuous = wrap("smooth", 
                      shape = 19,        # 实心圆点（无边框）
                      size = 0.8,
                      alpha = 0.85),
    # 组合变量：显示分面直方图
    combo = wrap(ggally_facethist, color = "black")
  )
) +  # ggpairs返回的是ggplot对象，可以使用ggplot2的语法进行修饰
  
  # 设置填充颜色
  scale_fill_manual(values = col) +
  
  # 设置边框/线条颜色
  scale_color_manual(values = col) +
  
  # 使用ggh4x包的函数添加次要坐标轴刻度
  guides(y = "axis_minor") +  
  
  # 设置主题为黑白主题
  theme_bw() +  
  
  # 进一步自定义主题
  theme(
    # 移除网格线
    panel.grid = element_blank(),
    
    # 设置x轴刻度标签大小
    axis.text.x = element_text(size = 11, angle = 45, hjust = 1,  vjust = 1)
  )


cor


ggsave(
  filename = "./11_cor_syn/Syn_相关性矩阵.pdf",  # 保存路径和文件名
  plot = cor,                                # 要保存的图形对象
  width = 9,                               # 图形宽度（英寸）
  height = 6,                               # 图形高度（英寸）
  device = "pdf",                           # 输出格式（可选）
  dpi = 600                                 # 分辨率（可选）
)



ggsave(
  filename = "./11_cor_syn/Syn_相关性矩阵.png",  # 保存路径和文件名
  plot = cor,                                # 要保存的图形对象
  width = 9,                               # 图形宽度（英寸）
  height = 6,                               # 图形高度（英寸）
  dpi = 600,                                # 分辨率，PNG需要设置高dpi
  bg = "white"                              # 背景色
)



#### Figure S4 ####

##### Figure S4A #####



rm(list = ls())
gc()


setwd("D:/work_year/1_data_analysis/6_plot_bac/")

class <- c("Sensitive", "Tolerant")

df_Class <- data.frame()
for (i in 1:length(class)){
 
  
  keyword <- class[i] 
  

  
  abd <- read.table(paste0("./4_ASV_abundance/0_data/4_", keyword, ".txt"), header = T, sep = "\t")
  sig <- read.csv(paste0("./4_ASV_abundance/0_data/3_", keyword, "_genus_significance_analysis.csv"), header = T)
  proportion <- read.table(paste0("./4_ASV_abundance/0_data/0_", keyword, "_ASV_abundance_Genus.txt"), header = T, sep = "\t")
  
  
  if (keyword == "Sensitive"){
    sig_1 <- sig[, c(1,2,9)]
    abd_1 <- abd[, c(1,6)]
    proportion_1 <-  proportion[,c(1,8,9)]
  } else if (keyword == "Tolerant" ) {
    
    sig_1 <- sig[, c(1,2,9)]
    abd_1 <- abd[, c(1,8)]
    proportion_1 <-  proportion[,c(1,10,11)]
    
  } else{
    sig_1 <- sig[, c(1,2,9)]
    abd_1 <- abd[, c(1,7)]
    proportion_1 <-  proportion[,c(1,9,10)]
    
  }
  
  
  cat("当分组为Sensitive, Tolerant时， 对照组为CK，处理组为Syn，\n 当分组为 CK|Syn时， 对照组（CK data）为 Tolerant, 处理组为sensitive")
  
  colnames(proportion_1) <- c("Genus", "percent_CK", "percent_treat")
  
  data_0 <- merge(abd_1, sig_1, by = "Genus", all.x = TRUE)  # all.x=TRUE保留A的所有行
  data_1 <- merge(data_0, proportion_1, by = "Genus", all.x = TRUE)  # all.x=TRUE保留A的所有行
  # 将 Jointing_data_1 中的所有 NA 替换为 0
  data_1[is.na(data_1)] <- 0 
  
  others_CK <- 100 - sum(data_1$percent_CK)
  others_treat <- 100 - sum(data_1$percent_treat)
  
  
  library(dplyr)
  data_1 <- data_1 %>%
    mutate(
      Treatment = ifelse(Genus == "g__Others", keyword, Treatment),  # 第3列
      Significance = ifelse(Genus == "g__Others", "ns", Significance),         # 第4列
      percent_CK = ifelse(Genus == "g__Others", others_CK,  percent_CK),
      percent_treat = ifelse(Genus == "g__Others", others_treat,  percent_treat)
      
    )
  
  
  if (i ==1 ){
    df_Class <- data_1
  } else {
    df_Class <- rbind( df_Class , data_1)
  }
  
}



write.table(df_Class, "4_ASV_abundance/0_data/5_drew_data.txt", sep = "\t", row.names = F, quote = F)

df_Class_1 <- df_Class[df_Class$Genus != "g__Others",]



library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)


# 读取数据（假设df_Stage是宽格式，包含percent_D/percent_W列）
df_long_all <- df_Class_1 %>%
  
  pivot_longer(
    cols = starts_with("percent_"),  
    names_to = "Class",
    values_to = "Value"
  ) %>%
  

  mutate(Genus = str_remove(Genus, "g__")) %>%

  mutate(Treatment = factor(Treatment, levels = c("Sensitive", "Tolerant", "CK", "Syn"))) %>%

  group_by(Treatment, Genus) %>%
  mutate(Mean_Abundance = mean(Value)) %>%
  ungroup() %>%

  mutate(is_others = ifelse(Genus == "Others", 1, 2)) %>%

  group_by(Treatment) %>%
  arrange(is_others, desc(Mean_Abundance)) %>%
  

  mutate(Genus = fct_inorder(Genus)) %>%
  ungroup() %>%

  mutate(Class = factor(
    Class, 
    levels = c("percent_CK", "percent_treat"), 
    labels = c("CK", "treat")
  ))
# 自定义调色板（用户提供）



mycolor <- c(
  
  "#1f77b4","#ff7f0e","#2ca02c","#d62728","#9467bd","#8c564b","#e377c2",
  "#7f7f7f","#bcbd22","#17becf","#aec7e8","#ffbb78","#98df8a","#ff9896",
  "#c5b0d5","#c49c94","#f7b6d2","#c7c7c7","#dbdb8d","#9edae5"
  
)

# 
cat("这里分两次运行")


# 2
df_long <- df_long_all[df_long_all$Treatment %in% c("Sensitive", "Tolerant"),]



# 获取所有属的因子水平（已按Others→大比例→小比例排序）
all_genera <- levels(df_long$Genus)

# 构建颜色映射（确保Others颜色正确）
color_map <- setNames(mycolor[1:length(all_genera)], all_genera)
color_map["Others"] <- "#F2F1F1"  # 强制覆盖，避免索引错误






ggplot(df_long, aes(x = Class, y = Value, fill = Genus)) +
  # 堆叠柱状图（按因子水平顺序：Others→大比例→小比例）
  geom_col(
    position = position_stack(reverse = FALSE), # 不反转，保持因子水平顺序
    color = "black",  # 柱子边框（增强层次感）
    width = 0.7,      # 柱子宽度（适配分面布局）
    alpha = 0.6       # 透明度（避免颜色过深）
  ) +
  # 分面显示（按生育期分组）
  
  facet_grid(. ~ Treatment) +
  
  # 应用颜色映射（反转图例顺序，对应堆叠的从上到下）
  scale_fill_manual(
    values = color_map, 
    breaks = levels(df_long$Genus),  # 反转图例，让"大比例属"对应堆叠顶部
    name = "Genus"
  ) +
  # Y轴优化（严格0-100%范围，无额外空间）
  scale_y_continuous(
    name = "Relative Abundance (%)",
    limits = c(0, 38),  # 强制0-100%
    expand = c(0, 0),    # 无额外空间
    breaks = seq(0, 38, 5)  # 刻度间隔20%
  ) +
  # 移除了X轴标题
  scale_x_discrete(name = NULL) +  # 设置为NULL以移除X轴标题
  # 主题设置（学术风格）
  theme_bw(base_size = 12) +
  theme(
    # 网格线（仅保留Y轴主网格）
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "gray90"),
    # 分面标题（白色背景+灰色边框）
    strip.background = element_rect(fill = "white", color = "gray70"),
    strip.text = element_text(face = "bold", size = 11),
    # 图例（右侧显示，标题粗体）
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    # 坐标轴（标题粗体，标签清晰）
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 12),
    # 布局（避免分面拥挤）
    panel.spacing = unit(1.0, "lines"),
    # 标题（居中，粗体）
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
  ) +
  # 标题与说明
  labs(
    title = "ASV Abundance Across Growth Stages",
    caption = "Stacking order: Others (bottom) → Genera sorted by mean abundance (descending) across treatments"
  )



ggsave(
  #   "./4_ASV_abundance/1_ASV-abd-Treatment.png", 
  "./4_ASV_abundance/1_ASV-abd-Drought-2.png",   
  width = 6,  # 宽度适配分面布局
  height = 5,   # 高度适配Y轴范围
  dpi = 600     # 高清分辨率（学术论文要求）
)


# 保存为PDF（矢量图，学术出版最佳格式）
ggsave(
  #  "./4_ASV_abundance/ASV-abd-Treatment.pdf",  # 修改为.pdf后缀
  "./4_ASV_abundance/ASV-abd-Drought-2.pdf",  # 修改为.pdf后缀
  width = 6,      # 7英寸宽度（学术论文常用单栏宽度）
  height = 5,     # 5英寸高度
  device = cairo_pdf,  # 使用cairo设备确保字体嵌入
  dpi = 600       # 高分辨率（虽然PDF是矢量格式，但会影响栅格元素）
)

dev.off()



#显著性绘图

rm(list = ls())
gc()


setwd("D:/work_year/1_data_analysis/6_plot_bac/")
class <- c("Sensitive", "Tolerant", "CK", "Syn")

# <- c( "Drought","Treatment")

group <- c( "Drought")

for (i in 1:length(group)){
  #   
  #    i <- 1
  
  keyword <- group[i]
  
  
  if (i == 1){
    sig_S <- read.csv(paste0("./4_ASV_abundance/0_data/3_Sensitive_genus_significance_analysis.csv"), header = T)
    sig_S_1 <- sig_S[, c(1,2,9)]
    colnames(sig_S_1) <- c("Genus","Sensitive_Stage","Sensitive_Significance")
    # 直接修改Genus列
    sig_S_1$Genus <- gsub("g__", "", sig_S_1$Genus)
    
    
    sig_T <- read.csv(paste0("./4_ASV_abundance/0_data/3_Tolerant_genus_significance_analysis.csv"), header = T)
    sig_T_1 <- sig_T[, c(1,2,9)]
    colnames(sig_T_1) <- c("Genus","Tolerant_Stage","Tolerant_Significance")
    sig_T_1$Genus <- gsub("g__", "", sig_T_1$Genus)
    
    
    order <- read.table("./4_ASV_abundance/0_data/6_order.txt",header = F)  # 根据ASV丰度图的图注确定
    colnames( order) <- "Genus"
    
    library(dplyr)

    merged_df <- order %>%
      left_join(sig_S_1, by = "Genus") %>%  # 第一次合并
      left_join(sig_T_1, by = "Genus")  # 第二次合并
    
    merged_df_1 <- merged_df[,c(1,3,5)]
    
    
    
    
    
    # 加载必要的R包
    library(tidyverse)  # 包含ggplot2, dplyr等
    library(viridis)    # 配色方案
    

    # 锁定原始Genus顺序（确保与输入数据框顺序完全一致）
    original_genus_order <- unique(merged_df_1$Genus)

    sig_df <- merged_df_1 %>%
      mutate(
        # 固定Genus顺序（不反转）
        Genus = factor(Genus, levels = original_genus_order),
        
        # 设置显著性因子水平（NA需要特殊处理）
        Sensitive_Significance = factor(
          Sensitive_Significance, 
          levels = c(NA, "ns", "*", "**", "***"), 
          exclude = NULL  # 保留NA值
        ),
        Tolerant_Significance = factor(
          Tolerant_Significance, 
          levels = c(NA, "ns", "*", "**", "***"), 
          exclude = NULL
        )
      )
    
    # 转换为长格式（ggplot2标准输入格式）
    sig_long <- sig_df %>%
      # 将宽格式转为长格式
      pivot_longer(
        cols = -Genus,                # 除Genus外的所有列
        names_to = "Class",           # 新列：生育期阶段
        values_to = "Significance"    # 新列：显著性值
      ) %>%
      # 清理阶段名称（移除"_Significance"后缀）
      mutate(
        Class = str_remove(Class, "_Significance"),
        # 固定阶段顺序（作物生长时序）
        Class = factor(Class, levels = c("Sensitive", "Tolerant"))
      )

    ggplot(sig_long, aes(x = Class, y = Genus, fill = Significance)) +
      
      # 1. 绘制矩阵单元格
      geom_tile(
        color = "black",    # 单元格边框颜色
        linewidth = 0.8,    # 边框线宽（ggplot2 3.4.0+语法）
        fill =  "white"            # 无填充色（透明）
      ) +
      
      # 2. 添加显著性标记文本
      geom_text(
        aes(label = Significance),  # 显示*/**/***/ns
        color = "black",           # 文本颜色
        fontface = "plain",         # 粗体
        size = 4                  # 字号
      ) +
      
      # 3. 配色设置（虽然不显示图例但仍需定义）
      scale_fill_viridis(
        option = "plasma",   # 使用viridis的plasma配色
        discrete = TRUE,     # 离散型数据
        direction = -1,      # 反转颜色顺序（使***最醒目）
        na.value = "gray90", # NA值显示为浅灰色
        guide = "none"       # 隐藏图例
      ) +
      
      # 4. 坐标轴设置
      scale_x_discrete(
        position = "bottom",  # X轴标签位置（底部）
        expand = c(0, 0)      # 无扩展（紧凑布局）
      ) +
      scale_y_discrete(
        limits = rev(original_genus_order),  # Y轴顺序控制
        expand = c(0, 0)      # 无扩展
      ) +
      
      # 5. 主题设置
      theme_minimal(base_size = 11) +  # 基础主题（字号12）
      theme(
        # 标题设置
        plot.title = element_text(
          hjust = 0.5,        # 水平居中
          face = "plain",      # 粗体
          size = 11          # 大字号
        ),
        
        # 坐标轴设置
        axis.title = element_blank(),  # 移除轴标题
        axis.text.x = element_text(
          face = "plain",      # 粗体
          size = 11,         # 字号
          margin = margin(t = 1)  # 上边距（避免文字贴图）
        ),
        axis.text.y = element_text(size = 10),  # Y轴文本
        
        # 网格线与背景
        panel.grid = element_blank(),  # 移除所有网格线
        # 关键修改：设置纯白背景
        panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8),  # 外边框
        plot.background = element_rect(fill = "white", color = "black", linewidth = 0.8),  # 最外层边框
        #   panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8),          # 绘图区边框  
        # 图例设置
        legend.position = "none",  # 彻底移除图例
        
        # 外边框
        panel.border = element_rect(
          fill = NA,              # 无填充
          color = "black",        # 黑色边框
          linewidth = 0.8           # 线宽
        )
      ) +
      
      # 6. 标签设置
      labs(
        title = "Significance Across Class"  # 主标题
      )

    ggsave(
      paste0("./4_ASV_abundance/","2_", keyword, "_significance_heatmap.png"),
      width = 5,    # 宽度（英寸）
      height = 5,   # 高度（根据属的数量可调整）
      dpi = 600      # 分辨率
    )
    
    # 可选：同时保存PDF格式（矢量图）
    ggsave(
      paste0("./4_ASV_abundance/","2_", keyword, "_significance_heatmap.pdf"),
      width = 5,
      height = 5,
      device = cairo_pdf  # 确保字体嵌入
    )
    
  
  } else {
    sig_S <- read.csv(paste0("./4_ASV_abundance/0_data/3_CK_genus_significance_analysis.csv"), header = T)
    sig_S_1 <- sig_S[, c(1,2,9)]
    colnames(sig_S_1) <- c("Genus","CK_Stage","CK_Significance")
    # 直接修改Genus列
    sig_S_1$Genus <- gsub("g__", "", sig_S_1$Genus)
    
    
    sig_T <- read.csv(paste0("./4_ASV_abundance/0_data/3_Syn_genus_significance_analysis.csv"), header = T)
    sig_T_1 <- sig_T[, c(1,2,9)]
    colnames(sig_T_1) <- c("Genus","Syn_Stage","Syn_Significance")
    sig_T_1$Genus <- gsub("g__", "", sig_T_1$Genus)
    
    
    order <- read.table("./4_ASV_abundance/0_data/6_order-Treatment.txt",header = F)  # 根据ASV丰度图的图注确定
    colnames( order) <- "Genus"
    
    
    library(dplyr)
    # 假设已有以下数据框：
    # order - 包含Genus列（基准列）
    # df1, df2, df3 - 每个数据框的第一列为Genus，后两列为数据
    merged_df <- order %>%
      left_join(sig_S_1, by = "Genus") %>%  # 第一次合并
      left_join(sig_T_1, by = "Genus")  # 第二次合并
    
    merged_df_1 <- merged_df[,c(1,3,5)]
    
    
    
    # 加载必要的R包
    library(tidyverse)  # 包含ggplot2, dplyr等
    library(viridis)    # 配色方案
    
  
    # 数据预处理部分
  
    
    # 锁定原始Genus顺序（确保与输入数据框顺序完全一致）
    original_genus_order <- unique(merged_df_1$Genus)
    
    # 数据清洗与格式化：
    # 1. 将Genus列转换为因子并固定顺序
    # 2. 对三个生育期的显著性进行因子化处理
    sig_df <- merged_df_1 %>%
      mutate(
        # 固定Genus顺序（不反转）
        Genus = factor(Genus, levels = original_genus_order),
        
        # 设置显著性因子水平（NA需要特殊处理）
        CK_Significance = factor(
          CK_Significance, 
          levels = c(NA, "ns", "*", "**", "***"), 
          exclude = NULL  # 保留NA值
        ),
        Syn_Significance = factor(
          Syn_Significance, 
          levels = c(NA, "ns", "*", "**", "***"), 
          exclude = NULL
        )
      )
    
    # 转换为长格式（ggplot2标准输入格式）
    sig_long <- sig_df %>%
      # 将宽格式转为长格式
      pivot_longer(
        cols = -Genus,                # 除Genus外的所有列
        names_to = "Class",           # 新列：生育期阶段
        values_to = "Significance"    # 新列：显著性值
      ) %>%
      # 清理阶段名称（移除"_Significance"后缀）
      mutate(
        Class = str_remove(Class, "_Significance"),
        # 固定阶段顺序（作物生长时序）
        Class = factor(Class, levels = c("CK", "Syn"))
      )
    
  
    # 可视化部分
  
    
    # 创建热图（矩阵图）
    ggplot(sig_long, aes(x = Class, y = Genus, fill = Significance)) +
      
      # 1. 绘制矩阵单元格
      geom_tile(
        color = "black",    # 单元格边框颜色
        linewidth = 0.8,    # 边框线宽（ggplot2 3.4.0+语法）
        fill =  "white"            # 无填充色（透明）
      ) +
      
      # 2. 添加显著性标记文本
      geom_text(
        aes(label = Significance),  # 显示*/**/***/ns
        color = "black",           # 文本颜色
        fontface = "plain",         # 粗体
        size = 4                  # 字号
      ) +
      
      # 3. 配色设置（虽然不显示图例但仍需定义）
      scale_fill_viridis(
        option = "plasma",   # 使用viridis的plasma配色
        discrete = TRUE,     # 离散型数据
        direction = -1,      # 反转颜色顺序（使***最醒目）
        na.value = "gray90", # NA值显示为浅灰色
        guide = "none"       # 隐藏图例
      ) +
      
      # 4. 坐标轴设置
      scale_x_discrete(
        position = "bottom",  # X轴标签位置（底部）
        expand = c(0, 0)      # 无扩展（紧凑布局）
      ) +
      scale_y_discrete(
        limits = rev(original_genus_order),  # Y轴顺序控制
        expand = c(0, 0)      # 无扩展
      ) +
      
      # 5. 主题设置
      theme_minimal(base_size = 11) +  # 基础主题（字号12）
      theme(
        # 标题设置
        plot.title = element_text(
          hjust = 0.5,        # 水平居中
          face = "plain",      # 粗体
          size = 11          # 大字号
        ),
        
        # 坐标轴设置
        axis.title = element_blank(),  # 移除轴标题
        axis.text.x = element_text(
          face = "plain",      # 粗体
          size = 11,         # 字号
          margin = margin(t = 1)  # 上边距（避免文字贴图）
        ),
        axis.text.y = element_text(size = 10),  # Y轴文本
        
        # 网格线与背景
        panel.grid = element_blank(),  # 移除所有网格线
        # 关键修改：设置纯白背景
        panel.background = element_rect(fill = "white", color = "black", linewidth = 0.8),  # 外边框
        plot.background = element_rect(fill = "white", color = "black", linewidth = 0.8),  # 最外层边框
        #   panel.border = element_rect(fill = NA, color = "black", linewidth = 0.8),          # 绘图区边框  
        # 图例设置
        legend.position = "none",  # 彻底移除图例
        
        # 外边框
        panel.border = element_rect(
          fill = NA,              # 无填充
          color = "black",        # 黑色边框
          linewidth = 0.8           # 线宽
        )
      ) +
      
      # 6. 标签设置
      labs(
        title = "Significance Across Class"  # 主标题
      )
    
  
    # 输出保存
  
    
    # 保存为PNG格式（600dpi高清）
    ggsave(
      paste0("./4_ASV_abundance/","2_", keyword, "_significance_heatmap.png"),
      width = 3,    # 宽度（英寸）
      height = 5,   # 高度（根据属的数量可调整）
      dpi = 600      # 分辨率
    )
    
    # 可选：同时保存PDF格式（矢量图）
    ggsave(
      paste0("./4_ASV_abundance/","2_", keyword, "_significance_heatmap.pdf"),
      width = 3,
      height = 5,
      device = cairo_pdf  # 确保字体嵌入
    )

    
  }

  
}



#####Figure S4B ####


rm(list = ls())
gc()

setwd("D:/work_year/1_data_analysis/6_plot_bac/") 

feature_table <- read.table('./6_RandomForest/0_data/otutab.txt',header=TRUE,row.names= 1,sep='\t',stringsAsFactors = F,quote="",check.name=F)
sample_table_0 <- read.table('./6_RandomForest/0_data/metadata.txt',header=TRUE,row.names= 1,sep='\t',stringsAsFactors = F,quote="",check.name=F)

sample_table <- sample_table_0[!grepl("_102", rownames(sample_table_0)), , drop = FALSE]

tax_table0 <- read.table('./6_RandomForest/0_data/taxtab.txt',header=TRUE,row.names= 1,sep='\t',stringsAsFactors = F,quote="",check.name=F)


# 批量去掉所有列的前缀
tax_table <- as.data.frame(apply(tax_table0, 2, function(x) {
  sub("^[dkpcofgs]__", "", x)
}))

# 如果原始是矩阵，需要转换
head(tax_table)


# 过滤掉Genus为空或为"-"的条目
tax_table <- tax_table[tax_table$Genus != "" & 
                         tax_table$Genus != "-" & 
                         !is.na(tax_table$Genus), ]


library(dplyr)
library(tidyr)

# 假设数据框名为 df
tax_table_1 <-  tax_table %>%
  # 合并1-7列，用"|"分隔，na.rm = TRUE：自动跳过空值（避免生成多余的 |）remove = FALSE：保留原始1-7列（若需删除改为 TRUE）
  unite("merged_column", 1:6, sep = "|", na.rm = TRUE, remove = FALSE) %>%
  # 替换连续多个"|"为单个"|"
  mutate(merged_column = gsub("\\|{2,}", "|", merged_column)) %>%
  # 移除首尾的"|"（可选）
  mutate(merged_column = gsub("^\\||\\|$", "", merged_column)) %>%
  # 行变列
  rownames_to_column("ASV")


tax_table_2 <- tax_table_1[, c(1,2)] 
# 查看结果
head(tax_table_2$merged_column)



#(1) sensitive 

sample_table_S <- sample_table[sample_table$Group2 %in% c("Sensitive_CK", "Sensitive_Syn"),]
feature_table_S <-  feature_table[,colnames(feature_table) %in% rownames(sample_table_S)]
dataset_S <- microtable$new(feature_table_S,sample_table_S,tax_table) 

set.seed(1234)
t3 <- trans_diff$new(dataset = dataset_S, 
                     method = "rf", 
                     group = "Group2", 
                     taxa_level = "Genus", 
                     filter_thres = 0,
                     alpha = 0.05,
                     p_adjust_method = "none")
results_S <- as.data.frame(t3$res_diff)
write.table(results_S,"./6_RandomForest/Sensitive_rf_Genus_0.05.txt",sep = "\t",col.names = T,row.names = T,quote = F)



library(dplyr)

# 1. 排序
df_gini <- results_S %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(rank = row_number())



# 二阶差分法（最直观）
gini <- df_gini$MeanDecreaseGini

d1 <- diff(gini)
d2 <- diff(d1)

elbow_rank_2nd  <- which.max(abs(d2)) + 1
elbow_value_2nd <- gini[elbow_rank_2nd]

elbow_rank_2nd
elbow_value_2nd




library(ggplot2)

p_s <-    ggplot(df_gini, aes(rank, MeanDecreaseGini)) +
  geom_line(color = "grey50", linewidth = 0.6) +
  geom_point(size = 1.2, alpha = 0.8) +
  geom_vline(
    xintercept = elbow_rank_2nd,
    linetype = "dashed",
    linewidth = 0.6,
    color = "red"
  ) +
  annotate(
    "text",
    x = elbow_rank_2nd,
    y = elbow_value_2nd,
    label = paste0("S_Elbow point (rank = ", elbow_rank_2nd, ")"),
    hjust = -0.05,
    size = 3,
    color = "red"
  ) +
  theme_classic(base_size = 11) +
  labs(
    x = "Ranked taxa",
    y = "Mean decrease in Gini index"
  )



ggsave(
  filename = "./6_RandomForest/s_RF_Gini_elbow.png",
  plot     = p_s,
  width    = 8,
  height   = 6,
  units    = "cm",
  dpi      = 300
)



ggsave(
  filename = "./6_RandomForest/s_RF_Gini_elbow.pdf",
  plot     = p_s,
  width    = 8,
  height   = 6,
  units    = "cm",
  device   = cairo_pdf
)

p_s


g3 <- t3$plot_diff_bar(use_number = 1:47,
                       group_order = c("Sensitive_CK", "Sensitive_Syn"),
                       group_two_sep = FALSE,  # 关键参数：不将两个组分开展示
                       group_aggre = TRUE,  # 保持分组聚合
                       coord_flip = FALSE, 
                       xtext_angle = 90, 
                       xtext_size = 8, 
                       ytext_size = 8
) +
  
  
  
  scale_fill_manual(values = c( "#EB9184", "#80ACF9")) + 
  scale_color_manual(values = c("#EB9184", "#80ACF9"))
g3

# 获取g3中分类单元的准确顺序
g3_taxa_order <- levels(g3$data$Taxa)  # 从图形数据中获取因子水平

# 在plot_diff_abund中使用order_x_mean = TRUE保持与g3相同的顺序
g4 <- t3$plot_diff_abund(group_order = c("Sensitive_CK", "Sensitive_Syn"), 
                         select_taxa = g3_taxa_order,  # 使用明确的顺序
                         order_x_mean = FALSE,  # 不按均值重新排序
                         order_x_by_g3 = TRUE,  # 如果函数支持自定义参数
                         coord_flip = FALSE,
                         add_sig = TRUE,
                         add_sig_increase = 0.01, 
                         add_sig_text_size = 1, 
                         xtext_angle = 90, 
                         xtext_size = 8, 
                         ytext_size = 8) +
  scale_fill_manual(values = c("#EB9184", "#80ACF9"), name = "Group") +
  scale_color_manual(values = c("#EB9184", "#80ACF9"), name = "Group")


g4

# 导出带标签图
rf <- gridExtra::grid.arrange(g4,g3,  ncol = 1, nrow = 2, heights = c(1.5, 1.5))
rf
ggsave("./6_RandomForest/Sensitive_rf_47_Genus_0.05-2.PDF",rf, width = 8, height = 5)

ggsave("./6_RandomForest/Sensitive_rf_47_Genus_0.05-2.png",rf, width = 8, height = 5,units = "in",
       dpi   = 300)



# 导出无标签图 
g3 <- g3 + theme(legend.position = "none")

g3 <- g3 + theme(axis.text.x = element_blank() 
)
g4 <- g4 + theme(axis.text.x = element_blank(), 
                 axis.ticks.x = element_blank())
g4 <- g4 + theme(legend.position = "none")

rf <- gridExtra::grid.arrange(g4,g3,  ncol = 1, nrow = 2, heights = c(1.0, 1.0), padding = unit(-5, "cm"))  # 增加图形之间的内边距
rf
ggsave("./6_RandomForest/Sensitive_rf_47_Genus_0.05.PDF",rf, width = 4, height = 2)

ggsave("./6_RandomForest/Sensitive_rf_47_Genus_0.05.png",rf, width = 4, height = 2,units = "in",
       dpi   = 300)









# (2) Tolerant

sample_table_T <- sample_table[sample_table$Group2 %in% c("Tolerant_CK", "Tolerant_Syn"),]
feature_table_T <-  feature_table[,colnames(feature_table) %in% rownames(sample_table_T)]
dataset_T <- microtable$new(feature_table_T,sample_table_T,tax_table) 
# dataset_T_2 <- microtable$new(feature_table_T,sample_table_T,tax_table0) 

set.seed(123)
t3 <- trans_diff$new(dataset = dataset_T, 
                     method = "rf", 
                     group = "Group2", 
                     taxa_level = "Genus", 
                     filter_thres = 0,
                     alpha = 0.05,
                     p_adjust_method = "none")
results_T <- as.data.frame(t3$res_diff)
write.table(results_T,"./6_RandomForest/Tolerant_rf_Genus_0.05.txt",sep = "\t",col.names = T,row.names = T,quote = F)


library(dplyr)

# 1. 排序
df_gini <- results_T %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(rank = row_number())



# 二阶差分法（最直观）
gini <- df_gini$MeanDecreaseGini

d1 <- diff(gini)
d2 <- diff(d1)

elbow_rank_2nd  <- which.max(abs(d2)) + 1
elbow_value_2nd <- gini[elbow_rank_2nd]

elbow_rank_2nd
elbow_value_2nd




library(ggplot2)

p_t <-  ggplot(df_gini, aes(rank, MeanDecreaseGini)) +
  geom_line(color = "grey50", linewidth = 0.6) +
  geom_point(size = 1.2, alpha = 0.8) +
  geom_vline(
    xintercept = elbow_rank_2nd,
    linetype = "dashed",
    linewidth = 0.6,
    color = "red"
  ) +
  annotate(
    "text",
    x = elbow_rank_2nd,
    y = elbow_value_2nd,
    label = paste0("T_Elbow point (rank = ", elbow_rank_2nd, ")"),
    hjust = -0.05,
    size = 3,
    color = "red"
  ) +
  theme_classic(base_size = 11) +
  labs(
    x = "Ranked taxa",
    y = "Mean decrease in Gini index"
  )


p_t
ggsave(
  filename = "./6_RandomForest/t_RF_Gini_elbow.png",
  plot     = p_t,
  width    = 8,
  height   = 6,
  units    = "cm",
  dpi      = 300
)



ggsave(
  filename = "./6_RandomForest/t_RF_Gini_elbow.pdf",
  plot     = p_t,
  width    = 8,
  height   = 6,
  units    = "cm",
  device   = cairo_pdf
)




g3 <- t3$plot_diff_bar(use_number = 1:36,
                       group_order = c("Tolerant_CK", "Tolerant_Syn"),  
                       group_two_sep = FALSE,  # 关键参数：不将两个组分开展示
                       group_aggre = TRUE,  # 保持分组聚合
                       coord_flip = FALSE, 
                       xtext_angle = 90, 
                       xtext_size = 8, 
                       ytext_size = 8
) +
  scale_fill_manual(values = c( "#EF9154", "#5C87B1")) + 
  scale_color_manual(values = c("#EF9154", "#5C87B1"))
g3


# 获取g3中分类单元的准确顺序
g3_taxa_order <- levels(g3$data$Taxa)  # 从图形数据中获取因子水平

g4 <- t3$plot_diff_abund(group_order = c("Tolerant_CK", "Tolerant_Syn"), 
                         select_taxa = g3_taxa_order,  # 使用明确的顺序
                         order_x_mean = FALSE,  # 不按均值重新排序
                         order_x_by_g3 = TRUE,  # 如果函数支持自定义参数
                         coord_flip = FALSE,
                         add_sig = TRUE,
                         add_sig_increase = 0.01, 
                         add_sig_text_size = 1, 
                         xtext_angle = 90, 
                         xtext_size = 8, 
                         ytext_size = 8) +
  scale_fill_manual(values = c("#EF9154", "#5C87B1"),
                    name = "Group") +
  scale_color_manual(values = c("#EF9154", "#5C87B1"),
                     name = "Group")

g4


rf <- gridExtra::grid.arrange(g4,g3,  ncol = 1, nrow = 2, heights = c(1.5, 1.5))
rf
ggsave("./6_RandomForest/Tolerance_rf_36_Genus_0.05-2.PDF",rf, width = 8, height = 15)

ggsave("./6_RandomForest/v_rf_36_Genus_0.05-2.png",rf, width = 8, height = 15,units = "in",
       dpi   = 300)


g3 <- g3 + theme(legend.position = "none")


g3 <- g3 + theme(axis.text.x = element_blank() 
)

g4 <- g4 + theme(axis.text.x = element_blank(), 
                 axis.ticks.x = element_blank())
g4 <- g4 + theme(legend.position = "none")

rf <- gridExtra::grid.arrange(g4,g3,  ncol = 1, nrow = 2, heights = c(1.0, 1.0), padding = unit(-5, "cm"))  
rf
ggsave("./6_RandomForest/Tolerance_rf_36_Genus_0.05.PDF",rf, width = 4, height = 2)

ggsave("./6_RandomForest/Tolerance_rf_36_Genus_0.05.png",rf, width = 4, height = 2,units = "in",
       dpi   = 300)





#### Figure S5 ####





rm(list = ls())
gc()

# 请确保运行环境的路径正确
setwd("D:/work_year/1_data_analysis/6_plot_bac/") 

dir.create("./7_Net-analysis", showWarnings = FALSE)
setwd("./7_Net-analysis")

# 导入所需R包
library(phyloseq)
library(igraph)
library(network)
library(sna)
library(tidyverse)
library(ggClusterNet)
library(ggnewscale)
library(tidyfst)

# 读取数据
cat("Loading data...\n")
feature_table <- read.table("../5_LEfSe/0_data/otutab.txt", header=TRUE, row.names=1, sep='\t', stringsAsFactors=F, quote="", check.names=F)
sample_table  <- read.table("../5_LEfSe/0_data/metadata.txt", header=TRUE, row.names=1, sep='\t', stringsAsFactors=F, quote="", check.names=F)
tax_table     <- read.table("../5_LEfSe/0_data/taxtab.txt", header=TRUE, row.names=1, sep='\t', stringsAsFactors=F, quote="", check.names=F)

# 构建全局 phyloseq 对象
ps0 = phyloseq(sample_data(sample_table),
               otu_table(as.matrix(feature_table), taxa_are_rows=TRUE),
               tax_table(as.matrix(tax_table)))


stage <- c("Sensitive", "Tolerant") 
all_net_stats_list <- list()        # 用于存储每个分组的统计指标

for (keyword in stage) {
  cat(paste0("\nProcessing: ", keyword, "\n"))
  
  # 创建目录
  dir.create(paste0("./", keyword, "/"), showWarnings = FALSE)
  path <- paste0("./", keyword, "/net_results/")
  dir.create(path, showWarnings = FALSE)
  
  # 提取子集与确定分组列
  if (any(c("Sensitive", "Tolerant") %in% keyword)) {
    ps <- subset_samples(ps0, Drought == keyword)
    group_var <- "Treatment" # 根据元数据，这一列会包含 Syn 和 CK
  } else {
    ps <- subset_samples(ps0, Treatment == keyword)
    group_var <- "Drought"
  }
  
  # 执行微生物共现网络分析
  set.seed(123)
  result = network.i(
    ps = ps,
    N = 1000,
    r.threshold = 0.8,
    big = T,
    select_layout = T,
    method = "spearman",
    scale = FALSE,
    layout_net = "model_igraph2",
    p.threshold = 0.05,
    label = FALSE,
    path = path,
    zipi = T,
    order = NULL,
    fill = "Phylum",
    group = group_var,  # 这里将根据 Syn 和 CK 输出两组网络结果
    ncol = 2,
    nrow = 2
  )
  
  # 保存网络图
  p_all_net = result[[1]]
  ggsave(paste0(keyword, "_1000_all_net.pdf"), path = path, width = 10, height = 8, limitsize = FALSE)
  ggsave(paste0(keyword, "_1000_all_net.png"), path = path, width = 10, height = 8, dpi = 300, limitsize = FALSE)
  
  p_all_net_1 = result[[3]]
  ggsave(paste0(keyword, "_1000_all_net-1.pdf"), path = path, width = 10, height = 8, limitsize = FALSE)
  ggsave(paste0(keyword, "_1000_all_net-1.png"), path = path, width = 10, height = 8, dpi = 300, limitsize = FALSE)
  
  # 保存网络性质数据表
  data_all_net = result[[2]]
  data_all_net_save <- data.frame(Infomation=rownames(data_all_net), data_all_net)
  write.table(data_all_net_save, file = paste0(path, keyword, "_1000_data_all_net.txt"), sep = '\t', col.names = T, row.names = F)
  
  # ★★★ 核心修复：宽数据转换为长数据，并拼接准确的样本名称 ★★★
  # 此时 data_all_net 的列名为 "Syn", "CK"
  temp_df <- as.data.frame(data_all_net) %>%
    rownames_to_column(var = "Metric") %>%
    pivot_longer(cols = -Metric, names_to = "SubGroup", values_to = "Value") %>%
    # 拼接 keyword(如 Sensitive) 和 SubGroup(如 Syn) 形成 Sensitive_Syn
    mutate(SampleType = paste0(keyword, "_", SubGroup)) %>% 
    select(Metric, Value, SampleType)
  
  all_net_stats_list[[keyword]] <- temp_df
}




cat("\nExporting 9 targeted topological properties...\n")

# 1. 依赖上游循环得到的 all_net_stats_list
data_raw <- bind_rows(all_net_stats_list)

# 2. 确保输出文件夹存在
dir.create("./TSS-Spearman-0.8", showWarnings = FALSE)

# (已剔除未计算的 Keystone_Nodes，保留实际存在的 9 个完美指标)
metric_mapping <- c(
  "RM(relative.modularity)" = "Modularity",                   # 1. 相对模块度
  "average.degree(Average K)" = "Avg_Degree",                 # 2. 平均度
  "num.vertices(n)" = "Nodes",                                # 3. 节点数
  "num.edges(L)" = "Edges",                                   # 4. 边数
  "Connectance(edge_density)" = "Density",                    # 5. 密度
  "mean.clustering.coefficient(Average.CC)" = "Clustering_Coeff", # 6. 聚类系数
  "average.path.length" = "Avg_Path_Length",                  # 7. 平均路径长度
  "diameter" = "Diameter",                                    # 8. 网络直径
  "no.clusters" = "Num_Clusters"                              # 9. 模块/社区数量
)
# 4. 清洗、映射并去重
data_export <- data_raw %>%
  filter(Metric %in% names(metric_mapping)) %>%           
  mutate(Metric = metric_mapping[Metric]) %>%             
  mutate(Value = as.numeric(Value)) %>%                   
  group_by(SampleType, Metric) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") 

# 5. 导出为本地文件，彻底解耦
write.table(data_export, file = "./TSS-Spearman-0.8/4_network_9metrics_exported.txt", 
            sep = '\t', col.names = TRUE, row.names = FALSE, quote = FALSE)

cat("\nData successfully exported to ./TSS-Spearman-0.8/4_network_9metrics_exported.txt\n")



rm(list = ls()) 
gc()

library(tidyverse)
library(ggplot2)

# 请确保工作路径一致
setwd("D:/work_year/1_data_analysis/6_plot_bac/") 

cat("\nReading data and plotting 8 metrics in a 2x4 layout...\n")

# 1. 读入解耦后的本地数据 (复用之前提取好的数据)
plot_data <- read.table("./7_Net-analysis/TSS-Spearman-0.8/4_network_9metrics_exported-bac.txt", 
                        header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# 2. 设定真实存在的 8 个指标
target_metrics <- c("Edges", "Avg_Degree", "Modularity", "Avg_Path_Length", 
                    "Nodes", "Density", "Clustering_Coeff", "Diameter")
plot_data <- plot_data %>% filter(Metric %in% target_metrics)
plot_data$Metric <- factor(plot_data$Metric, levels = target_metrics)

# 3. 设定样本因子顺序
expected_samples <- c("Sensitive_CK", "Sensitive_Syn", "Tolerant_CK", "Tolerant_Syn")
plot_data$SampleType <- factor(plot_data$SampleType, levels = expected_samples)

# 4. 颜色分配
metric_colors <- c(
  "Modularity"       = "#E762D7",
  "Avg_Degree"       = "#6ABD8C",
  "Nodes"            = "#4C8EDA",
  "Edges"            = "#E38E62",
  "Density"          = "#D35E8C",
  "Clustering_Coeff" = "#F0C04A",
  "Avg_Path_Length"  = "#5E9BD3",
  "Diameter"         = "#B16B6B"
)

# 5. 区分浮点型和整型指标
float_metrics <- c("Modularity", "Avg_Degree", "Density", "Clustering_Coeff", "Avg_Path_Length", "Diameter")

# 6. 开始绘制版式
p <- ggplot(plot_data, aes(x = SampleType, y = Value)) +
  geom_point(aes(color = Metric), size = 2.5, alpha = 0.85) +  # 点稍微放大让2x4更饱满
  
  geom_text(
    aes(
      label = ifelse(
        Metric %in% float_metrics,
        sprintf("%.2f", Value),  # 浮点型指标保留2位小数
        sprintf("%.0f", Value)   # 整型指标不保留小数
      ),
      vjust = -1.2
    ),
    size = 2.5,
    fontface = "bold",
    color = "black",
    position = position_nudge(y = 0.05)
  ) +
  
  # ★ 核心修改：改为 2 行 4 列的完美排版 ★
  facet_wrap(
    ~ Metric,                   
    scales = "free_y",          
    ncol = 4,                   # 设定为4列，自动换成2行
    strip.position = "top"
  ) +  
  
  scale_color_manual(values = metric_colors, guide = "none") +
  
  labs(
    x = "",
    y = "Value",
    title = "Fungal Network Key Topological Properties"
  ) +
  
  scale_y_continuous(
    limits = function(x) {
      if (length(na.omit(x)) == 0 || max(x, na.rm = TRUE) == 0) return(c(0, 1))
      c(max(x, na.rm = TRUE) * -0.1, max(x, na.rm = TRUE) * 1.25)  # 顶部留白 25% 防重叠
    },
    expand = expansion(mult = c(-0.05, 0.15)) 
  ) +
  
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 12)),
    axis.text.x = element_text(size = 9, angle = 45, vjust = 1.0, hjust = 1, face = "plain", color = "black"),
    axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title.y = element_text(size = 10, face = "bold", margin = margin(r = 10)),
    panel.spacing.x = unit(4.0, "mm"), # 加宽左右间距
    panel.spacing.y = unit(6.0, "mm"), # 增加上下行间距，防止x轴标签遮挡上一行
    strip.text = element_text(size = 9, face = "plain", margin = margin(t = 3, b = 3)),
    strip.background = element_rect(fill = "grey95", color = "black", linewidth = 0.5),
    panel.grid.major = element_line(color = "grey92", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "none",
    panel.border = element_rect(fill = NA, color = "black", linewidth = 0.5),
    axis.ticks.length = unit(0.15, "cm"),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    plot.margin = margin(15, 15, 15, 15)
  )

print(p)

# ★ 图像保存：调整尺寸为宽度 10 英寸，高度 5 英寸（极佳的 2:1 宽高比）
ggsave("./7_Net-analysis/TSS-Spearman-0.8/fungal_network_metrics_2x4.pdf", plot = p, width = 7, height = 3.8, units = "in")
ggsave("./7_Net-analysis/TSS-Spearman-0.8/fungal_network_metrics_2x4.png", plot = p, width = 7, height = 3.8, units = "in", dpi = 300, bg = "white")

cat("\nPlot complete! Check the TSS-Spearman-0.8 folder.\n")






#### Figure S6 #####

"Same as Figure 3"



#### Figure S7 #### 




rm(list = ls())
gc()
setwd("D:/work_year/5_宏基因组-RNAseq/3_RNA-seq-analysis/")

cat("\n=== 00 加载依赖包 ===\n")
library(DESeq2)
library(limma)
library(pheatmap)


# 第一部分：全局数据读取与科学预处理 (Count & TPM 统一对齐)

cat("\n=== 01 数据读取与预处理优化 ===\n")

# 1.1 处理 Count 矩阵
count_data <- read.csv("./4_count_ext/gene_count_matrix.csv", row.names=1, check.names=FALSE)
rownames(count_data) <- gsub("^gene:", "", rownames(count_data)) # 去除冗余前缀

# 【数据清洗】：安全处理 NA 值，将其视为未检测到(0)，杜绝 na.omit 误删整行
if(any(is.na(count_data))) {
  cat("警告：Count 矩阵中存在 NA，已自动替换为 0\n")
  count_data[is.na(count_data)] <- 0
}

# 移除全零基因 (粗筛)
zero_genes <- rowSums(count_data) == 0
count_data <- count_data[!zero_genes, ]
cat("Count 矩阵 -> 移除全零基因:", sum(zero_genes), "/ 保留有效基因:", nrow(count_data), "\n")


# 1.2 处理 TPM 矩阵 (提取到全局，避免在后续循环中重复读取消耗内存)
TPM_data <- read.csv("./5_Norm-count_ext/tpm_matrix.csv", row.names=1, check.names=FALSE)
rownames(TPM_data) <- gsub("^gene:", "", rownames(TPM_data))

# 安全处理 NA
if(any(is.na(TPM_data))) TPM_data[is.na(TPM_data)] <- 0

# 移除全零基因
TPM_data <- TPM_data[rowSums(TPM_data) > 0, ]

# 【严格对齐】：锁定 TPM 的样本列序与 count_data 完全一致，防止索引错乱
common_samples <- intersect(colnames(count_data), colnames(TPM_data))
count_data <- count_data[, common_samples]
TPM_data <- TPM_data[, common_samples]
cat("TPM 矩阵 -> 全局对齐完成，包含样本:", ncol(TPM_data), "个，基因:", nrow(TPM_data), "个\n")




cat("\n=== 02 开始 DESeq2 核心计算 ===\n")

# 2.1 提取样本分组信息 
group_list <- gsub("_[0-9]+$", "", common_samples) # 增强正则，兼容诸如 _10 的双位数重复
group <- factor(group_list, levels = c("R_Syn", "R_CK", "R_H18", "R_H20"))
colData <- data.frame(row.names = common_samples, condition = group)

# 2.2 构建 DESeqDataSet 对象 (由于是 count 数据，必须四舍五入确保为整数)
dds <- DESeqDataSetFromMatrix(countData = round(as.matrix(count_data)),
                              colData = colData,
                              design = ~ condition)

# 2.3 预过滤 (严格生物学筛选：至少在 3 个样本中 count >= 10，去除低表达随机噪音)
keep <- rowSums(counts(dds) >= 10) >= 3
dds <- dds[keep,]
cat("DESeq2 严苛过滤后保留:", nrow(dds), "个基因\n")

# 2.4 执行差异分析核心流程 (估算大小因子、离散度并进行负二项分布检验)
dds <- DESeq(dds)


cat("=== 03 正在生成 QC 可视化图表 ===\n")
if(!dir.exists("./6_plot")) dir.create("./6_plot", recursive = TRUE)

# 【QC-1: 离散度图】
pdf("./6_plot/DESeq2-Dispersion_Plot.pdf", width = 7, height = 6)
plotDispEsts(dds, main = "DESeq2 Dispersion Estimates")
dev.off()

png("./6_plot/DESeq2-Dispersion_Plot.png", width = 7, height = 6, units = "in", res = 300)
plotDispEsts(dds, main = "DESeq2 Dispersion Estimates")
dev.off()

# 获取 VST (方差稳定变换) 标准化矩阵用于 MDS 和 Heatmap，降低均值-方差依赖
vsd <- vst(dds, blind = FALSE)
vst_matrix <- assay(vsd)

# 定义全局统一的绘图色彩与形状样式
my_colors_vec <- c("R_CK" = "#E41A1C", "R_H18" = "#377EB8", "R_H20" = "#4DAF4A", "R_Syn" = "#984EA3")
my_shapes_vec <- c("R_CK" = 16, "R_H18" = 17, "R_H20" = 18, "R_Syn" = 15)
annotation_col <- data.frame(Group = group, row.names = colnames(dds))
ann_colors <- list(Group = my_colors_vec)

# 【QC-2: MDS 降维散点图】
mds_result <- plotMDS(vst_matrix, plot = FALSE)
plot_colors <- my_colors_vec[as.character(group)]
plot_shapes <- my_shapes_vec[as.character(group)]
x_margin <- diff(range(mds_result$x)) * 0.15
y_margin <- diff(range(mds_result$y)) * 0.15

mds_func <- function() {
  par(mar = c(5, 5, 4, 2))
  plot(mds_result$x, mds_result$y, col = plot_colors, pch = plot_shapes, cex = 2,
       xlab = "Dimension 1", ylab = "Dimension 2",
       main = "MDS Plot (based on VST data)",
       xlim = c(min(mds_result$x) - x_margin, max(mds_result$x) + x_margin),
       ylim = c(min(mds_result$y) - y_margin, max(mds_result$y) + y_margin), las = 1,
       panel.first = grid(lty = "dotted", col = "gray")) # 底层虚线网格
  text(mds_result$x, mds_result$y, labels = colnames(dds), pos = 3, cex = 0.8, col = plot_colors)
  legend("topright", legend = levels(group), col = my_colors_vec[levels(group)],
         pch = my_shapes_vec[levels(group)], pt.cex = 1.5, cex = 0.9, bty = "n")
}

pdf("./6_plot/DESeq2-MDS_plot.pdf", width = 7, height = 6)
mds_func(); dev.off()
png("./6_plot/DESeq2-MDS_plot.png", width = 7, height = 6, units = "in", res = 300)
mds_func(); dev.off()

# 【QC-3: 样本相关性热图】
cor_matrix <- cor(vst_matrix)
pheatmap(cor_matrix, annotation_col = annotation_col, annotation_colors = ann_colors,
         main = "Sample Correlation Heatmap (VST)",
         cluster_rows = TRUE, cluster_cols = TRUE, display_numbers = TRUE, number_format = "%.3f",
         color = colorRampPalette(c("#377EB8", "white", "#E41A1C"))(100),
         filename = "./6_plot/DESeq2-sample_correlation.pdf", width = 8, height = 6)
pheatmap(cor_matrix, annotation_col = annotation_col, annotation_colors = ann_colors,
         main = "Sample Correlation Heatmap (VST)",
         cluster_rows = TRUE, cluster_cols = TRUE, display_numbers = TRUE, number_format = "%.3f",
         color = colorRampPalette(c("#377EB8", "white", "#E41A1C"))(100),
         filename = "./6_plot/DESeq2-sample_correlation.png", width = 8, height = 6)



# 第三部分：差异分析循环提取与绘图 (区分 ENSRNA 与 Gene)

cat("\n=== 04 开始提取两两比较结果并绘图 ===\n")

# 匹配以 "ENSRNA" 开头的非编码 RNA (小麦注释库)
ncRNA_pattern <- "^ENSRNA" 


library(ggplot2)
library(ggrastr)
library(pheatmap)


res_dir_ck <- "./7_DESeq2_results/CK_control_sep/"
if(!dir.exists(res_dir_ck)) dir.create(res_dir_ck, recursive = TRUE)

treatments_vs_ck <- c("R_Syn", "R_H18", "R_H20")
cur_ref_ck <- "R_CK" 

for (treat in treatments_vs_ck) {
  cur_treat <- as.character(treat)
  cat(paste0("\n>>> [CK 作为对照] 正在处理: ", cur_treat, " vs ", cur_ref_ck, " ...\n"))
  
  # A. 提取结果
  res_obj <- results(dds, contrast = c("condition", cur_treat, cur_ref_ck))
  res <- as.data.frame(res_obj)
  res$FDR <- res$padj
  res$logFC <- res$log2FoldChange
  
  # B. 判定差异基因 (强鲁棒性：过滤 FDR < 0.05 且 |logFC| > 1)
  res$change <- "Stable"
  res$change[which(res$FDR < 0.05 & res$logFC > 1)]  <- "Up"
  res$change[which(res$FDR < 0.05 & res$logFC < -1)] <- "Down"
  
  # C. 区分类型 (打上 Gene 或 ncRNA 标签)
  res$gene_type <- "Gene" 
  res$gene_type[grepl(ncRNA_pattern, rownames(res), ignore.case = TRUE)] <- "ncRNA"
  
  write.csv(res, paste0(res_dir_ck, "/", cur_treat, "_vs_CK_DEG_results.csv"))
  
  # D. 绘图数据与颜色映射准备
  res_plot <- res
  res_plot$FDR[is.na(res_plot$FDR)] <- 1 
  res_plot$log10FDR <- -log10(res_plot$FDR)
  
  # 精准构建 4 组分类标签
  res_plot$group <- "Stable"
  res_plot$group[res_plot$change == "Up" & res_plot$gene_type == "Gene"] <- "Gene Up"
  res_plot$group[res_plot$change == "Down" & res_plot$gene_type == "Gene"] <- "Gene Down"
  res_plot$group[res_plot$change == "Up" & res_plot$gene_type == "ncRNA"] <- "ncRNA Up"
  res_plot$group[res_plot$change == "Down" & res_plot$gene_type == "ncRNA"] <- "ncRNA Down"
  
  # 动态计算包含具体基因数的四组图例标签
  leg_labels <- c(
    paste0("Gene Up: ", sum(res_plot$group == "Gene Up", na.rm=T)), 
    paste0("Gene Down: ", sum(res_plot$group == "Gene Down", na.rm=T)),
    paste0("ncRNA Up: ", sum(res_plot$group == "ncRNA Up", na.rm=T)), 
    paste0("ncRNA Down: ", sum(res_plot$group == "ncRNA Down", na.rm=T))
  )
  leg_colors <- c("Gene Up" = "#E41A1C", "Gene Down" = "#377EB8", "ncRNA Up" = "#0303FD", "ncRNA Down" = "#FF7F00")
  

  max_logfc_volcano <- max(abs(res_plot$logFC), na.rm = TRUE) * 1.1 
  
  volcano_func_ck <- function() {
    # 将稳定点与差异点剥离，Stable 作为灰色背景，其余 4 组严格映射到图例
    df_stable <- subset(res_plot, group == "Stable")
    df_sig <- subset(res_plot, group != "Stable")
    df_sig$group <- factor(df_sig$group, levels = c("Gene Up", "Gene Down", "ncRNA Up", "ncRNA Down"))
    
    p <- ggplot() +
      theme_bw() +
      theme(
        panel.grid.major = element_line(linetype = "dashed", color = "grey85"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 11, color = "#111111", face = "bold"),
        axis.title.y = element_text(size = 11, color = "#111111", face = "bold"),
        axis.text = element_text(size = 11, color = "#333333"),
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size = 5),
        # 👇 添加这行代码来控制图标与文本的距离
        legend.spacing.x = unit(-2, "cm")
      ) +
      geom_vline(xintercept = c(-1, 1), color = "black", linetype = "dashed") +
      geom_hline(yintercept = -log10(0.05), color = "black", linetype = "dashed") +
      # 1. 栅格化底层灰色背景点
      ggrastr::rasterise(geom_point(data = df_stable, aes(x = logFC, y = log10FDR), color = "grey80", size = 1.3, alpha = 0.5, shape = 16), dpi = 300) +
      # 2. 栅格化核心 4 组差异点
      ggrastr::rasterise(geom_point(data = df_sig, aes(x = logFC, y = log10FDR, color = group), size = 1.3, alpha = 0.8, shape = 16), dpi = 300) +
      scale_color_manual(
        values = leg_colors,
        labels = leg_labels,
        drop = FALSE # 即使某组缺失也严格保留位置
      ) +
      scale_x_continuous(limits = c(-max_logfc_volcano, max_logfc_volcano)) +
      labs(title = paste0("Volcano Plot: ", cur_treat, " vs ", cur_ref_ck),
           x = "log2(Fold Change)", y = "-log10(FDR)")
    print(p)
  }
  
  pdf(paste0(res_dir_ck, "/Volcano_", cur_treat, "_vs_CK.pdf"), 3, 3)
  volcano_func_ck(); dev.off()
  png(paste0(res_dir_ck, "/Volcano_", cur_treat, "_vs_CK.png"), 3, 3, units="in", res=300)
  volcano_func_ck(); dev.off()
  

  treat_samples <- grep(paste0("^", cur_treat, "_"), colnames(TPM_data), value = TRUE)
  shared_genes <- intersect(rownames(res), rownames(TPM_data)) 
  res_plot_scat <- res_plot[shared_genes, ]
  
  # 计算 Y 轴表达丰度
  res_plot_scat$mean_log2tpm <- rowMeans(log2(TPM_data[shared_genes, treat_samples, drop=FALSE] + 1))
  max_logfc_scat <- max(abs(res_plot_scat$logFC), na.rm = TRUE) * 1.1 
  
  scatter_func_ck <- function() {
    df_stable_scat <- subset(res_plot_scat, group == "Stable")
    df_sig_scat <- subset(res_plot_scat, group != "Stable")
    df_sig_scat$group <- factor(df_sig_scat$group, levels = c("Gene Up", "Gene Down", "ncRNA Up", "ncRNA Down"))
    
    p <- ggplot() +
      theme_bw() +
      theme(
        panel.grid.major = element_line(linetype = "dashed", color = "grey85"),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 11, color = "#111111", face = "bold"),
        axis.title.y = element_text(size = 11, color = "#111111", face = "bold"),
        axis.text = element_text(size = 11, color = "#333333"),
        plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        legend.position = c(0.98, 0.98),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text = element_text(size = 5),
        # 👇 添加这行代码来控制图标与文本的距离
        legend.spacing.x = unit(-2, "cm")
      ) +
      geom_vline(xintercept = c(-1, 1), color = "black", linetype = "dashed") +
      # 1. 栅格化底层稳定点
      ggrastr::rasterise(geom_point(data = df_stable_scat, aes(x = logFC, y = mean_log2tpm), color = "grey80", size = 1.3, alpha = 0.5, shape = 16), dpi = 300) +
      # 2. 栅格化 4 组差异表达点
      ggrastr::rasterise(geom_point(data = df_sig_scat, aes(x = logFC, y = mean_log2tpm, color = group), size = 1.3, alpha = 0.8, shape = 16), dpi = 300) +
      scale_color_manual(
        values = leg_colors,
        labels = leg_labels,
        drop = FALSE
      ) +
      scale_x_continuous(limits = c(-max_logfc_scat, max_logfc_scat)) +
      labs(title = paste0("Scatter Plot: ", cur_treat, " vs ", cur_ref_ck),
           x = "log2(Fold Change)", y = paste0("Mean log2(TPM+1) in ", cur_treat))
    print(p)
  }
  
  pdf(paste0(res_dir_ck, "/Scatter_", cur_treat, "_vs_CK.pdf"), 3, 3)
  scatter_func_ck(); dev.off()
  png(paste0(res_dir_ck, "/Scatter_", cur_treat, "_vs_CK.png"), 3, 3, units="in", res=300)
  scatter_func_ck(); dev.off()
  

  }
  
 
}




#### Figure S8 ####


rm(list = ls())
gc()

# 1. 设置工作目录 (已做脱敏处理，请根据实际情况调整盘符路径)
setwd("D:/work_year/5_宏基因组-RNAseq/3_RNA-seq-analysis/")

# 加载必要包
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpubr)      # 用于散点图添加相关性方程和R值
library(patchwork)   # 用于完美拼接不同坐标轴比例的双Y轴图

# 定义候选基因及颜色图谱
candidate <- c("TraesCS2D02G317800","TraesCS2B02G109700",
               "TraesCS5B02G488700","TraesCS7A02G258800","TraesCS2A02G026600",
               "TraesCS4A02G229100")

treatment_colors_map <- c(
  "CK"       = "#00A087",  # 科研青绿
  "Syn"      = "#4DBBD5",  # 亮蓝
  "Syn_Pse." = "#FB6E1C",  # 正红
  "Syn_Sph." = "#F39B7F"   # 橘粉
)

cat("=== 01 RNA-seq TPM 数据读取与预处理 ===\n")
TPM_data <- read.csv("./5_Norm-count_ext/tpm_matrix.csv", row.names=1, check.names=FALSE)
rownames(TPM_data) <- gsub("^gene:", "", rownames(TPM_data))
zero_genes <- apply(TPM_data, 1, function(x) all(x == 0))
TPM_data <- TPM_data[!zero_genes, ]
cat("移除全零基因:", sum(zero_genes), "/ 保留有效基因:", nrow(TPM_data), "\n")

cat("=== 02 RNA-seq DEG 差异分析数据读取 ===\n")
DEG_H18 <- read.csv("./7_DESeq2_results/Syn_control_sep/R_H18_vs_Syn_DEG_results.csv", header = TRUE)
DEG_H20 <- read.csv("./7_DESeq2_results/Syn_control_sep/R_H20_vs_Syn_DEG_results.csv", header = TRUE)
DEG_CK  <- read.csv("./7_DESeq2_results/Syn_control_sep/R_CK_vs_Syn_DEG_results.csv", header = TRUE)

cat("=== 03 qPCR 定量数据读取与映射 ===\n")
# 注意路径根据你的实际存放位置修改
data <- read.table("./8-定量/drew-data.txt", header = TRUE, sep = "\t")

data <- data %>%
  rename(geneID = gene) %>%
  mutate(
    geneID = as.character(geneID),
    geneID = case_when(
      geneID == "7800" ~ "TraesCS2D02G317800",
      geneID == "9700" ~ "TraesCS2B02G109700",
      geneID == "3100" ~ "TraesCS4D02G063100",
      geneID == "8700" ~ "TraesCS5B02G488700",
      geneID == "8800" ~ "TraesCS7A02G258800",
      geneID == "6600" ~ "TraesCS2A02G026600",
      geneID == "9100" ~ "TraesCS4A02G229100",
      TRUE ~ geneID
    ),
    group = case_when(
      group == "H18" ~ "Syn_Pse.",
      group == "H20" ~ "Syn_Sph.",
      TRUE ~ as.character(group)
    )
  )

=============================== #
#                     数据清洗与联合准备阶段
=============================== #

# --- 处理 DEG 数据 ---
# 过滤候选基因并统一组别命名
DEG_H18_sub <- DEG_H18 %>% filter(X %in% candidate) %>% mutate(group = "Syn_Pse.")
DEG_H20_sub <- DEG_H20 %>% filter(X %in% candidate) %>% mutate(group = "Syn_Sph.")
DEG_CK_sub  <- DEG_CK  %>% filter(X %in% candidate) %>% mutate(group = "CK")

DEG_all <- bind_rows(DEG_H18_sub, DEG_H20_sub, DEG_CK_sub) %>%
  select(geneID = X, group, RNAseq_log2FC = log2FoldChange)

# --- 处理 qPCR 数据进行 FC 相关性计算 ---
# qPCR 的 log2FC 为 -ddct。我们计算三个重复 ddct 的平均值的相反数
qPCR_fc <- data %>%
  filter(group != "Syn") %>% # 去除对照组（自身对数倍数变化为0）
  rowwise() %>%
  mutate(
    mean_ddct = mean(c(ddct_1, ddct_2, ddct_3), na.rm = TRUE),
    qPCR_log2FC = -mean_ddct
  ) %>%
  ungroup() %>%
  select(geneID, group, qPCR_log2FC)

# 合并 FC 数据用于绘制散点图
corr_data <- inner_join(DEG_all, qPCR_fc, by = c("geneID", "group"))

# --- 处理 TPM 与 qPCR 丰度用于双 Y 轴图 ---
tpm_sub <- TPM_data[rownames(TPM_data) %in% candidate, ]
tpm_sub$geneID <- rownames(tpm_sub)
tpm_long <- tpm_sub %>%
  pivot_longer(cols = -geneID, names_to = "sample", values_to = "TPM") %>%
  mutate(
    group = case_when(
      grepl("CK", sample) ~ "CK",
      grepl("H18", sample) ~ "Syn_Pse.",
      grepl("H20", sample) ~ "Syn_Sph.",
      grepl("Syn", sample) ~ "Syn",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(geneID, group) %>%
  summarise(
    TPM_mean = mean(TPM, na.rm = TRUE),
    TPM_sd = sd(TPM, na.rm = TRUE),
    .groups = "drop"
  )

qPCR_bar <- data %>% select(geneID, group, qPCR_mean = mean, qPCR_sd = SD)

dual_data <- inner_join(qPCR_bar, tpm_long, by = c("geneID", "group"))
dual_data$group <- factor(dual_data$group, levels = c("Syn", "CK", "Syn_Pse.", "Syn_Sph."))



p_corr <- ggplot(corr_data, aes(x = RNAseq_log2FC, y = qPCR_log2FC)) +
  geom_point(aes(color = group, shape = group), size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", color = "black", fill = "grey80", linetype = "dashed") +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +
  scale_color_manual(values = treatment_colors_map) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey50") +
  labs(
    title = "Correlation of Gene Expression Fold Changes",
    x = expression("RNA-seq "*log[2]*"(Fold Change)"),
    y = expression("qPCR "*log[2]*"(Fold Change) [-"*Delta*Delta*"C"["T"]*"]")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, color = "black"),
    legend.title = element_blank(),
    legend.position = "right"
  )

print(p_corr)
ggsave("./8-定量/1_Log2FC_Correlation_Scatter.pdf", plot = p_corr, width = 3.8, height = 2.8)
ggsave("./8-定量/1_Log2FC_Correlation_Scatter.png", plot = p_corr, width = 3.8, height = 2.8, dpi = 300)



#### Figure S9 ####



#### Figure S9A-D  ####


library(ggplot2)
library(ggpubr)
library(stringr)
library(dplyr)       # 新增：用于数据分组计算
library(agricolae)   # 新增：用于多重比较和生成显著性字母

# --- 1. 数据准备 ---
alpha_df <- read.csv("genus_Alpha_Diversity_Index_Table.csv", row.names = TRUE)


alpha_df$Sample <- rownames(alpha_df)
alpha_df$Group <- str_split_fixed(alpha_df$Sample, "_", 2)[, 1]
alpha_df$Group <- factor(alpha_df$Group, levels = c("Syn","CK",  "H18", "H20"))

my_colors <- c("CK" = "#4DBBD5", "Syn" = "#E64B35", "H18" = "#00A087", "H20" = "#3C5488")

# --- 2. 定义循环参数 ---
metrics <- c("Observed", "Shannon", "Simpson", "Evenness")

# 定义测试方法：这里只保留 ANOVA + Tukey HSD
test_methods <- list(
  list(global = "anova",        label = "ANOVA")
  # list(global = "kruskal.test", label = "Kruskal") # 若只需ANOVA可将其注释
)


# --- 3. 嵌套循环绘图与合并 ---
for (test in test_methods) {
  
  cat("\n========== 开始处理:", test$label, "检验 ==========\n")
  plot_list <- list() # 创建一个空列表，用于存放当前检验方法下的所有单图
  
  for (metric in metrics) {
    cat("  正在绘制单图:", metric, "\n")
    
    # 动态 Y 轴标签
    y_label <- switch(metric,
                      "Observed" = "Observed Richness",
                      "Shannon"  = "Shannon Index",
                      "Evenness" = "Pielou's Evenness (J)",
                      "ACE"      = "ACE Index",
                      metric) # 默认返回本身
    

    formula_str <- paste0(metric, " ~ Group")
    
    if (test$global == "anova") {
      # ANOVA + Tukey HSD
      model <- aov(as.formula(formula_str), data = alpha_df)
      posthoc <- agricolae::HSD.test(model, "Group", group = TRUE)
    } else {
      # Kruskal-Wallis + Wilcoxon rank sum (作为备用保留)
      posthoc <- agricolae::kruskal(alpha_df[[metric]], alpha_df$Group, p.adj = "BH", group = TRUE)
    }
    
    # 提取字母并整理数据框
    sig_letters <- posthoc$groups
    sig_letters$Group <- rownames(sig_letters)
    
    # 计算每个组的最大 Y 值，以便把字母放在箱线图正上方
    letter_pos <- alpha_df %>%
      group_by(Group) %>%
      summarise(Max_Y = max(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
      left_join(sig_letters, by = "Group")
    

    p <- ggplot(alpha_df, aes(x = Group, y = .data[[metric]], fill = Group)) +
      stat_boxplot(geom = "errorbar", width = 0.2, linewidth = 0.8) + 
      geom_boxplot(alpha = 0.8, outlier.shape = NA, linewidth = 0.8) + 
      geom_jitter(width = 0.15, shape = 21, size = 1.0, color = "gray20", fill = "gray", stroke = 1) +
      scale_fill_manual(values = my_colors) +
      theme_classic(base_size = 11) + 
      labs(title = paste0(metric, " (", test$label, " Test)"), x = "", y = y_label) +
      # 动态留出顶部 15% 的空白区域，防止字母被天花板裁剪
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.15))) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        legend.position = "none", 
        axis.text.x = element_text(face = "bold", color = "black", size = 11),
        axis.text.y = element_text(face = "plain", color = "black", size = 11),
        axis.line = element_line(linewidth = 0.8)
      ) +
      
      # A. 添加显著性字母标记 (悬浮在最大值上方)
      geom_text(data = letter_pos, 
                aes(x = Group, y = Max_Y + (max(Max_Y) * 0.01), label = groups), 
                size = 3.5, fontface = "bold", color = "black", inherit.aes = FALSE) +
      
      # B. 全局 P 值标注 (保持放在右下角)
      stat_compare_means(method = test$global, 
                         label.x.npc = 0.95, 
                         label.y.npc = 0.05, 
                         hjust = 1,      # 右对齐
                         size = 3, 
                         fontface = "italic", 
                         color = "red")  # 用红色突出全局检验
    
    # 将生成的图存入列表
    plot_list[[metric]] <- p
    
    # --- 4. 单图多格式导出 ---
    base_path <- paste0("./2_analysis_R/1_species/1_alpha_div/Bracken/", keyword, "/","Alpha_", metric, "_", test$label)
    
    # 保存单图 PDF & PNG
    ggsave(paste0(base_path, ".pdf"), plot = p, width = 4, height = 3.5, dpi = 300)
    ggsave(paste0(base_path, ".png"), plot = p, width = 4, height = 3.5, dpi = 300)
  }
  
  # --- 5. 合并并导出多图 ---
  cat("  正在合并所有指标并导出组合图...\n")
  
  # 使用 ggpubr 的 ggarrange 合并图形
  merged_plot <- ggarrange(plotlist = plot_list, 
                           ncol = 4, nrow = 1, 
                           labels = c("A", "B", "C", "D"),
                           font.label = list(size = 11, face = "bold"))
  
  merged_base_path <- paste0("./2_analysis_R/1_species/1_alpha_div/Bracken/", keyword, "/","Alpha_Merged_All_", test$label)
  
  # 保存合并后的 PDF 和 PNG
  ggsave(paste0(merged_base_path, "-2.pdf"), plot = merged_plot, width = 8.1, height = 1.8, dpi = 300)
  ggsave(paste0(merged_base_path, "-2.png"), plot = merged_plot, width = 8.1, height = 1.8, dpi = 300)
  
}

cat("\n所有任务已完成！\n文件保存在: ./2_analysis_R/1_species/1_alpha_div/Bracken/\n")


}










#### Figure S9E-H ####


alpha_df <- read.csv("genus_Alpha_Diversity_Index_Table.csv", row.names = TRUE)
alpha_df$Sample <- rownames(alpha_df)
alpha_df$Group <- str_split_fixed(alpha_df$Sample, "_", 2)[, 1]
alpha_df$Group <- factor(alpha_df$Group, levels = c("Syn","CK",  "H18", "H20"))

my_colors <- c("CK" = "#4DBBD5", "Syn" = "#E64B35", "H18" = "#00A087", "H20" = "#3C5488")

# --- 2. 定义循环参数 ---
metrics <- c("Observed", "Shannon", "Simpson", "Evenness")

# 定义测试方法：这里只保留 ANOVA + Tukey HSD
test_methods <- list(
  list(global = "anova", label = "ANOVA")
  # 如果后续还想对比非参数检验，可以取消下面这行的注释
  # , list(global = "kruskal.test", label = "Kruskal")
)



for (test in test_methods) {
  
  cat("\n========== 开始处理:", test$label, "检验 ==========\n")
  plot_list <- list() # 创建一个空列表，用于存放当前检验方法下的所有单图
  
  for (metric in metrics) {
    cat("  正在绘制单图:", metric, "\n")
    
    # 动态 Y 轴标签
    y_label <- switch(metric,
                      "Observed" = "Observed Richness",
                      "Shannon"  = "Shannon Index",
                      "Evenness" = "Pielou's Evenness (J)",
                      "ACE"      = "ACE Index",
                      metric) # 默认返回本身
    
  
    formula_str <- paste0(metric, " ~ Group")
    
    if (test$global == "anova") {
      # ANOVA + Tukey HSD
      model <- aov(as.formula(formula_str), data = alpha_df)
      posthoc <- agricolae::HSD.test(model, "Group", group = TRUE)
    } else {
      # Kruskal-Wallis + Wilcoxon rank sum (作为备用选项保留)
      posthoc <- agricolae::kruskal(alpha_df[[metric]], alpha_df$Group, p.adj = "BH", group = TRUE)
    }
    
    # 提取字母并整理数据框
    sig_letters <- posthoc$groups
    sig_letters$Group <- rownames(sig_letters)
    
    # 计算每个组的最大 Y 值，以便把字母放在箱线图正上方
    letter_pos <- alpha_df %>%
      group_by(Group) %>%
      summarise(Max_Y = max(.data[[metric]], na.rm = TRUE), .groups = "drop") %>%
      left_join(sig_letters, by = "Group")
    

    p <- ggplot(alpha_df, aes(x = Group, y = .data[[metric]], fill = Group)) +
      stat_boxplot(geom = "errorbar", width = 0.2, linewidth = 0.8) + 
      geom_boxplot(alpha = 0.8, outlier.shape = NA, linewidth = 0.8) + 
      geom_jitter(width = 0.15, shape = 21, size = 1.0, color = "gray20", fill = "gray", stroke = 1) +
      scale_fill_manual(values = my_colors) +
      theme_classic(base_size = 11) + 
      labs(title = paste0(metric, " (", test$label, " Test)"), x = "", y = y_label) +
      # 动态留出顶部 15% 的空白区域，防止字母被天花板裁剪
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.15))) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
        legend.position = "none", 
        axis.text.x = element_text(face = "bold", color = "black", size = 11),
        axis.text.y = element_text(face = "plain", color = "black", size = 11),
        axis.line = element_line(linewidth = 0.8)
      ) +
      
      # A. 添加显著性字母标记 (悬浮在最大值上方)
      geom_text(data = letter_pos, 
                aes(x = Group, y = Max_Y + (max(Max_Y) * 0.03), label = groups), 
                size = 3.5, fontface = "bold", color = "black", inherit.aes = FALSE) +
      
      # B. 全局 P 值标注 (保持放在右下角)
      stat_compare_means(method = test$global, 
                         label.x.npc = 0.95, 
                         label.y.npc = 0.05, 
                         hjust = 1,      # 右对齐
                         size = 3, 
                         fontface = "italic", 
                         color = "red")  # 用红色突出全局检验
    
    # 将生成的图存入列表
    plot_list[[metric]] <- p
    
    # --- 4. 单图多格式导出 ---
    base_path <- paste0("./2_analysis_R/1_species/1_alpha_div/Bracken/", keyword, "/","Alpha_", metric, "_", test$label)
    
    # 保存单图 PDF & PNG
    ggsave(paste0(base_path, ".pdf"), plot = p, width = 4, height = 3.5, dpi = 300)
    ggsave(paste0(base_path, ".png"), plot = p, width = 4, height = 3.5, dpi = 300)
  }
  
  # --- 5. 合并并导出多图 ---
  cat("  正在合并所有指标并导出组合图...\n")
  
  # 使用 ggpubr 的 ggarrange 合并图形
  merged_plot <- ggarrange(plotlist = plot_list, 
                           ncol = 4, nrow = 1, 
                           labels = c("A", "B", "C", "D"),
                           font.label = list(size = 11, face = "bold"))
  
  merged_base_path <- paste0("./2_analysis_R/1_species/1_alpha_div/Kaiju_fungi/", keyword, "/","Alpha_Merged_All_", test$label)
  
  # 保存合并后的 PDF 和 PNG
  ggsave(paste0(merged_base_path, "-2.pdf"), plot = merged_plot, width = 8.1, height = 1.8, dpi = 300)
  ggsave(paste0(merged_base_path, "-2.png"), plot = merged_plot, width = 8.1, height = 1.8, dpi = 300)
  
  
}

}

cat("\n所有任务已完成！\n文件保存在: ./2_analysis_R/1_species/1_alpha_div/Kaiju_fungi/\n")



#### Figure S9I ####




rm(list = ls())
gc()

# 加载必要的包
library(phyloseq)
library(ggplot2)
library(ggpubr)
library(vegan)      # 用于 PERMANOVA 分析
library(microbiome) # 用于 CLR 标准化和过滤

# 设置工作目录
setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")

# 输出目录设置
output_dir <- "./2_analysis_R/1_species/2_beta_div/Bracken"

if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

class <- c( "genus")

# 创建子目录
dir.create(file.path(output_dir, "species"), showWarnings = FALSE)
dir.create(file.path(output_dir, "genus"), showWarnings = FALSE)

# 循环处理 species 和 genus 层级
for (i in 1:length(class)) {
  
  # i <- 1
  
  
  keyword <- class[i]
  cat("\n=============================================\n")
  cat("Processing level:", keyword, "...\n")
  
  # 1. 读取上一步清理好的 ps 对象
  ps <- readRDS(paste0("./2_analysis_R/1_species/0_data/2_Bracken_", keyword, "_cleaned.rds"))
  

  cat("【1】执行过滤...\n")
  cat("过滤前物种数:", ntaxa(ps), "\n")
  
  # 普遍性过滤：针对 12 个样本，保留至少在 2 个样本 (约 15%) 中出现的物种
  # 这步对于减少 CLR 转换时的零值伪计数噪音极其重要
  min_prevalence <- 2
  keep_taxa <- taxa_names(ps)[rowSums(otu_table(ps) > 0) >= min_prevalence]
  ps_filtered <- prune_taxa(keep_taxa, ps)
  
  cat("过滤后物种数:", ntaxa(ps_filtered), "\n")
  

  cat("【2】执行 CLR 转换...\n")
  # transform="clr" 会自动对包含 0 的数据添加极小的伪计数，并进行中心化对数比转换
  ps_clr <- microbiome::transform(ps_filtered, transform = "clr")
  

  cat("【3】计算 Aitchison 距离并运行 PERMANOVA...\n")
  
  # CLR 转换后的欧式距离即为 Aitchison 距离
  dist_matrix <- phyloseq::distance(ps_clr, method = "euclidean")
  
  # 提取 metadata 用于统计检验
  meta <- as(sample_data(ps_clr), "data.frame")
  
  # 运行 Adonis2 (PERMANOVA) 检验 Group 间差异
  set.seed(123) # 设置随机种子以保证结果可重复
  adonis_res <- adonis2(dist_matrix ~ Group, data = meta, permutations = 999)
  print(adonis_res)
  
  # 提取统计值用于绘图
  r2_val <- round(adonis_res$R2[1], 3)
  p_val <- adonis_res$`Pr(>F)`[1]
  stat_text <- paste0("PERMANOVA (Adonis): R2 = ", r2_val, ", P = ", p_val)
  
  # 注意：adonis2 返回的是特殊的 anova 对象，需要先强转为数据框
  adonis_df <- as.data.frame(adonis_res)
  
  # 定义 CSV 保存路径 (按 species/genus 自动分类)
  csv_file <- file.path(output_dir, keyword, paste0("PERMANOVA_Result_", keyword, ".csv"))
  
  # 写入 CSV 文件，保留行名（即 Group, Residuals, Total 等）
  write.csv(adonis_df, file = csv_file, row.names = TRUE)
  

  cat("【4】生成 PCoA 图...\n")
  pcoa_res <- ordinate(ps_clr, method = "PCoA", distance = "euclidean")
  
  # --- 新增核心代码：提取各轴的解释度百分比 ---
  # pcoa_res$values$Relative_eig 储存了每个主坐标轴的相对特征值（即解释方差比例）
  pc1_var <- round(pcoa_res$values$Relative_eig[1] * 100, 2)
  pc2_var <- round(pcoa_res$values$Relative_eig[2] * 100, 2)
  
  # 拼接你想要的标签文本
  pc1_label <- paste0("PCoA1 (", pc1_var, "%)")
  pc2_label <- paste0("PCoA2 (", pc2_var, "%)")

  
  # 如果没有安装 ggrepel，请先运行: install.packages("ggrepel")
  library(ggrepel) # 引入 ggrepel 包以防止标签重叠
  
  p_pcoa <- plot_ordination(ps_clr, pcoa_res, color = "Group") +
    geom_point(size = 3, alpha = 0.9) + 
    
    # --- 【核心修改】新增：添加防重叠的文本标签 ---
    # 注意：请将 "SampleID" 替换为你 metadata 中实际想要显示为标签的列名
    # 如果你想带背景框的标签，可以把 geom_text_repel 换成 geom_label_repel
    geom_text_repel(
      aes(label = SampleID), 
      size = 3,               # 标签文字大小
      show.legend = FALSE,    # 避免标签字母 "a" 出现在图例中
      max.overlaps = 20,      # 允许的最大重叠次数，可根据点数适当调大
      box.padding = 0.5       # 标签到数据点的缓冲距离
    ) +
    
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
      plot.subtitle = element_text(size = 12, hjust = 0.5), # 调整副标题大小
      
      # --- 优化坐标轴刻度文字 (Ticks) ---
      axis.text = element_text(color = "black", size = 12),
      
      # --- 优化坐标轴标题 (Axis Titles) ---
      axis.title = element_text(face = "plain", color = "black", size = 13),
      
      legend.position = "right",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13, face = "bold")
    ) +
    labs(
      title = paste("PCoA based on Aitchison distance (", keyword, ")", sep=""),
      subtitle = stat_text,
      x = pc1_label,
      y = pc2_label
    )
  
  # 打印图像
  print(p_pcoa)
  
  # 保存 PCoA 图 (代码保持不变)
  ggsave(filename = file.path(output_dir, keyword, paste0("PCoA_", keyword, ".pdf")), 
         plot = p_pcoa, width = 4, height = 3)
  ggsave(filename = file.path(output_dir, keyword, paste0("PCoA_", keyword, ".png")), 
         plot = p_pcoa, width = 4, height = 3, dpi = 300)
  

}



#### Figure S9J ####


rm(list = ls())
gc()

# 加载必要的包
library(phyloseq)
library(ggplot2)
library(ggpubr)
library(vegan)      # 用于 PERMANOVA 分析
library(microbiome) # 用于 CLR 标准化和过滤

# 设置工作目录
setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")

# 输出目录设置
output_dir <- "./2_analysis_R/1_species/2_beta_div/Kaiju_fungi"

if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

class <- c("species", "genus")

# 创建子目录
dir.create(file.path(output_dir, "species"), showWarnings = FALSE)
dir.create(file.path(output_dir, "genus"), showWarnings = FALSE)

# 循环处理 species 和 genus 层级
for (i in 1:length(class)) {
  
  # i <- 1
  
  
  keyword <- class[i]
  cat("\n=============================================\n")
  cat("Processing level:", keyword, "...\n")
  
  # 1. 读取上一步清理好的 ps 对象
  ps <- readRDS(paste0("./2_analysis_R/1_species/0_data/2_Kaiju_", keyword, "_cleaned_fungi.rds"))

  cat("【1】执行过滤...\n")
  cat("过滤前物种数:", ntaxa(ps), "\n")
  
  # 普遍性过滤：针对 12 个样本，保留至少在 2 个样本 (约 15%) 中出现的物种
  # 这步对于减少 CLR 转换时的零值伪计数噪音极其重要
  min_prevalence <- 2
  keep_taxa <- taxa_names(ps)[rowSums(otu_table(ps) > 0) >= min_prevalence]
  ps_filtered <- prune_taxa(keep_taxa, ps)
  
  cat("过滤后物种数:", ntaxa(ps_filtered), "\n")

  cat("【2】执行 CLR 转换...\n")
  # transform="clr" 会自动对包含 0 的数据添加极小的伪计数，并进行中心化对数比转换
  ps_clr <- microbiome::transform(ps_filtered, transform = "clr")

  cat("【3】计算 Aitchison 距离并运行 PERMANOVA...\n")
  
  # CLR 转换后的欧式距离即为 Aitchison 距离
  dist_matrix <- phyloseq::distance(ps_clr, method = "euclidean")
  
  # 提取 metadata 用于统计检验
  meta <- as(sample_data(ps_clr), "data.frame")
  
  # 运行 Adonis2 (PERMANOVA) 检验 Group 间差异
  set.seed(123) # 设置随机种子以保证结果可重复
  adonis_res <- adonis2(dist_matrix ~ Group, data = meta, permutations = 999)
  print(adonis_res)
  
  # 提取统计值用于绘图
  r2_val <- round(adonis_res$R2[1], 3)
  p_val <- adonis_res$`Pr(>F)`[1]
  stat_text <- paste0("PERMANOVA (Adonis): R2 = ", r2_val, ", P = ", p_val)
  
  # 注意：adonis2 返回的是特殊的 anova 对象，需要先强转为数据框
  adonis_df <- as.data.frame(adonis_res)
  
  # 定义 CSV 保存路径 (按 species/genus 自动分类)
  csv_file <- file.path(output_dir, keyword, paste0("PERMANOVA_Result_", keyword, ".csv"))
  
  # 写入 CSV 文件，保留行名（即 Group, Residuals, Total 等）
  write.csv(adonis_df, file = csv_file, row.names = TRUE)
  

  
  cat("【4】生成 PCoA 图...\n")
  pcoa_res <- ordinate(ps_clr, method = "PCoA", distance = "euclidean")
  
  # --- 新增核心代码：提取各轴的解释度百分比 ---
  # pcoa_res$values$Relative_eig 储存了每个主坐标轴的相对特征值（即解释方差比例）
  pc1_var <- round(pcoa_res$values$Relative_eig[1] * 100, 2)
  pc2_var <- round(pcoa_res$values$Relative_eig[2] * 100, 2)
  
  # 拼接你想要的标签文本
  pc1_label <- paste0("PCoA1 (", pc1_var, "%)")
  pc2_label <- paste0("PCoA2 (", pc2_var, "%)")

  
  # 如果尚未加载，请先加载 ggrepel 包
  library(ggrepel) 
  
  p_pcoa <- plot_ordination(ps_clr, pcoa_res, color = "Group") +
    geom_point(size = 3, alpha = 0.9) + 
    
    # --- 新增：添加防重叠的文本标签 ---
    geom_text_repel(
      aes(label = SampleID),  # ⚠️请确保将 "SampleID" 替换为你实际的样本名称列！
      size = 2.5,             # 因为你的画幅 (3.2x2.8) 较小，这里建议将标签字号调小至 2.5 或更小
      show.legend = FALSE,    # 避免图例中出现杂乱的字母 "a"
      max.overlaps = 50,      # 提高允许的重叠阈值，防止在小画幅中标签被强制隐藏
      box.padding = 0.3,      # 减小缓冲距离，适应小画幅
      segment.size = 0.3      # 牵引线调细一点
    ) +
    
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
      plot.subtitle = element_text(size = 12, hjust = 0.5), # 调整副标题大小
      
      # --- 优化坐标轴刻度文字 (Ticks) ---
      axis.text = element_text(color = "black", size = 12),
      
      # --- 优化坐标轴标题 (Axis Titles) ---
      axis.title = element_text(face = "plain", color = "black", size = 13),
      
      legend.position = "right",
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 13, face = "bold")
    ) +
    labs(
      title = paste("PCoA based on Aitchison distance (", keyword, ")", sep=""),
      subtitle = stat_text,
      x = pc1_label,
      y = pc2_label
    )

  ggsave(filename = file.path(output_dir, keyword, paste0("PCoA_", keyword, ".pdf")), 
         plot = p_pcoa, width = 4, height = 3)
  
  ggsave(filename = file.path(output_dir, keyword, paste0("PCoA_", keyword, ".png")), 
         plot = p_pcoa, width = 4, height = 3, dpi = 300)

 
}


#### Figure S9K ####


rm(list = rm(list = ls())) 
gc()            


suppressWarnings(suppressMessages({
  library(phyloseq)   # 微生物组数据处理基石
  library(vegan)      # 生态学统计核心包 (包含 Mantel 和 Procrustes)
  library(ggplot2)    # 高质量绘图引擎
  library(dplyr)      # 数据清洗
  library(ggsci)      # 提供顶刊配色
  library(ggExtra)    # 用于绘制 Mantel 边缘密度图
  library(agricolae)  # 用于多重比较与添加显著性字母
  library(DescTools)  # 新增：用于执行 DunnettTest
}))

# 设置工作目录到项目根文件夹 (请根据实际电脑路径修改)
setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")

# 定义全局大输出目录
main_out_dir <- "./2_analysis_R/1_species/7_crosskingdom/2_CLR"
if(!dir.exists(main_out_dir)) dir.create(main_out_dir, recursive = TRUE)


# 模块 2: 全局参数控制台

class_levels <- c( "genus") 
group_col <- "Group" # 实验分组列名

group_order <- c("Syn", "CK", "H18", "H20") 


# 模块 3: 启动大循环：遍历每个分类级别

for (i in 1:length(class_levels)){
  
  keyword <- class_levels[i]
  cat(sprintf("\n🚀 正在处理层级: %s...\n", toupper(keyword)))
  
  dir_class <- paste0(main_out_dir, "/", keyword)
  outDir_Mantel <- paste0(dir_class, "/Mantel")
  outDir_Procrustes <- paste0(dir_class, "/Procrustes")
  
  if(!dir.exists(outDir_Mantel)) dir.create(outDir_Mantel, recursive = TRUE)
  if(!dir.exists(outDir_Procrustes)) dir.create(outDir_Procrustes, recursive = TRUE)
  

  # 步骤 1. 数据读取与初步样本对齐

  ps_fungi <- readRDS(paste0("./2_analysis_R/1_species/0_data/2_Kaiju_", keyword, "_cleaned_fungi.rds"))
  ps_bac <- readRDS(paste0("./2_analysis_R/1_species/0_data/2_Bracken_", keyword, "_cleaned.rds"))
  
  otu_bac <- as(otu_table(ps_bac), "matrix")
  if(taxa_are_rows(ps_bac)) otu_bac <- t(otu_bac)
  
  otu_fun <- as(otu_table(ps_fungi), "matrix")
  if(taxa_are_rows(ps_fungi)) otu_fun <- t(otu_fun)
  
  common_samples <- intersect(rownames(otu_bac), rownames(otu_fun))
  if (length(common_samples) < 3) {
    cat("⚠️ 警告：初始有效共有样本不足 3 个，跳过此层级。\n")
    next
  }
  
  otu_bac <- otu_bac[common_samples, , drop = FALSE]
  otu_fun <- otu_fun[common_samples, , drop = FALSE]
  
 
  # 步骤 1.5: 跨界分析前的数据低丰度与低频过滤
 
  cat("    ✂️ 正在进行低丰度与低频特征过滤...\n")
  
  # 设定过滤阈值（0.0001 = 0.01%相对丰度; 0.10 = 在至少10%样本中出现）
  min_rel_abund <- 0.00001 
  min_prevalence <- 0.10  
  
  # 1. 过滤细菌矩阵
  rel_bac <- decostand(otu_bac, method = "total")
  keep_bac <- (colMeans(rel_bac) >= min_rel_abund) & 
    (colSums(otu_bac > 0) / nrow(otu_bac) >= min_prevalence)
  otu_bac <- otu_bac[, keep_bac, drop = FALSE]
  
  # 2. 过滤真菌矩阵
  rel_fun <- decostand(otu_fun, method = "total")
  keep_fun <- (colMeans(rel_fun) >= min_rel_abund) & 
    (colSums(otu_fun > 0) / nrow(otu_fun) >= min_prevalence)
  otu_fun <- otu_fun[, keep_fun, drop = FALSE]
  
  # 3. 二次清理：移除过滤特征后可能出现的总丰度为 0 的极端“空样本”
  valid_samples_bac <- rowSums(otu_bac) > 0
  valid_samples_fun <- rowSums(otu_fun) > 0
  valid_common <- valid_samples_bac & valid_samples_fun
  
  if (sum(valid_common) < 3) {
    cat("⚠️ 警告：特征过滤后，有效样本不足 3 个，跳过此层级。\n")
    next
  }
  
  # 更新最终对齐的矩阵和样本名
  otu_bac <- otu_bac[valid_common, , drop = FALSE]
  otu_fun <- otu_fun[valid_common, , drop = FALSE]
  common_samples <- rownames(otu_bac)
  
  cat(sprintf("    📊 [%s] 过滤后保留特征数 -> 细菌: %d, 真菌: %d. 最终有效样本: %d\n", 
              toupper(keyword), ncol(otu_bac), ncol(otu_fun), length(common_samples)))
  
  # 4. 提取元数据分组信息 (采用更健壮的双重逻辑)
  meta_df <- as(sample_data(ps_bac), "data.frame")[common_samples, , drop = FALSE]
  if(group_col %in% colnames(meta_df)) {
    sample_groups <- meta_df[[group_col]]
  } else {
    sample_groups <- gsub("_[0-9]+$", "", common_samples)
  }
  

  # 步骤 2. 最优数据标准化 (CLR 转换 + Aitchison 距离)

  cat("    🔄 正在进行 CLR 转换并计算 Aitchison 距离...\n")
  
  # 计算伪计数 (非零最小值的二分之一)
  pseudo_bac <- min(otu_bac[otu_bac > 0]) / 2 
  pseudo_fun <- min(otu_fun[otu_fun > 0]) / 2
  
  # 一步到位：使用 vegan 的 pseudocount 参数直接进行 CLR 转换
  otu_bac_clr <- decostand(otu_bac, method = "clr", pseudocount = pseudo_bac)
  otu_fun_clr <- decostand(otu_fun, method = "clr", pseudocount = pseudo_fun)
  
  # CLR 转换后的欧几里得距离即为 Aitchison 距离
  dist_bac <- vegdist(otu_bac_clr, method = "euclidean")
  dist_fun <- vegdist(otu_fun_clr, method = "euclidean")
  
 
  
 
  # 步骤 4: Procrustes 普氏分析与多维出图
 
  cat("    >>> 正在进行 Procrustes Analysis...\n")
  
  pcoa_bac <- cmdscale(dist_bac, k = 2)
  pcoa_fun <- cmdscale(dist_fun, k = 2)
  
  pro_res <- procrustes(X = pcoa_bac, Y = pcoa_fun, symmetric = TRUE)
  pro_test <- protest(X = pcoa_bac, Y = pcoa_fun, permutations = 999)
  
  pro_stats <- data.frame(
    Level = keyword, M_squared = pro_res$ss, Correlation_r = pro_test$t0, 
    P_value = pro_test$signif, Permutations = pro_test$permutations
  )
  write.csv(pro_stats, paste0(outDir_Procrustes, "/Procrustes_Stats_", keyword, ".csv"), row.names = FALSE)
  
  # 提取真实残差值与坐标
  Y_rotated <- pro_res$Yrot 
  X_target <- pro_res$X     
  sample_residuals <- residuals(pro_res)
  
  pro_plot_df <- data.frame(
    Sample = common_samples,
    Group = sample_groups,
    Bac_PCoA1 = X_target[, 1], Bac_PCoA2 = X_target[, 2],
    Fun_PCoA1 = Y_rotated[, 1], Fun_PCoA2 = Y_rotated[, 2],
    Residuals = sample_residuals
  )
  
  # 核心修改：强制对数据框中的 Group 设为因子，且指定既定顺序
  pro_plot_df <- pro_plot_df %>% 
    mutate(Group = factor(Group, levels = group_order))
  
  write.csv(pro_plot_df, paste0(outDir_Procrustes, "/Procrustes_Coordinates_and_Residuals_", keyword, ".csv"), row.names = FALSE)
  
  # 【经典可视化 1】：普氏残差旋转连线图
  p_pro_rot <- ggplot(pro_plot_df) +
    geom_segment(
      aes(x = Fun_PCoA1, y = Fun_PCoA2, xend = Bac_PCoA1, yend = Bac_PCoA2, color = Group), 
      arrow = arrow(length = unit(0.25, "cm"), type = "closed"), 
      alpha = 0.5, linewidth = 0.7
    ) +
    geom_point(
      aes(x = Bac_PCoA1, y = Bac_PCoA2, fill = Group, shape = "Bacterial Community"), 
      size = 3, color = "black", stroke = 0.3, alpha = 0.9 
    ) +
    geom_point(
      aes(x = Fun_PCoA1, y = Fun_PCoA2, fill = Group, shape = "Fungal Community"), 
      size = 2.5, color = "black", stroke = 0.3, alpha = 0.8
    ) +
    scale_shape_manual(
      name = "Kingdom", 
      values = c("Bacterial Community" = 21, "Fungal Community" = 24)
    ) +
    theme_bw(base_size = 13) +
    labs(
      title = paste("Procrustes Analysis:", tools::toTitleCase(keyword)),
      subtitle = sprintf("r = %.3f, p = %.3f (M² = %.3f)", pro_test$t0, pro_test$signif, pro_res$ss),
      x = "Procrustes Axis 1", 
      y = "Procrustes Axis 2", 
      color = "Treatment",
      fill = "Treatment" 
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 13),
      plot.subtitle = element_text(face = "italic", color = "grey30", hjust = 0.5, size = 13),
      axis.title.x = element_text(face = "bold", color = "black", size = 13, margin = margin(t = 10)),
      axis.title.y = element_text(face = "bold", color = "black", size = 13, margin = margin(r = 10)),
      axis.text = element_text(color = "black", size = 13),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 12)
    ) +
    guides(
      shape = guide_legend(override.aes = list(
        color = "black", 
        fill = "grey50", 
        size = 4, 
        stroke = 0.5     
      )),
      color = guide_legend(override.aes = list(size = 3, shape = 16)),
      fill = "none" 
    )
  
  ggsave(paste0(outDir_Procrustes, "/1_Procrustes_RotationPlot_", keyword, ".pdf"), plot = p_pro_rot, width = 6.5, height = 4)
  ggsave(paste0(outDir_Procrustes, "/1_Procrustes_RotationPlot_", keyword, ".png"), plot = p_pro_rot, width = 6.5, height = 4, dpi = 300)
  
 
  # 【修改可视化 4】：处理组间残差箱线图 (参数检验 ANOVA + Tukey HSD 与 DunnettTest)
 
  
  # 1. 计算全局整体差异显著性 (One-way ANOVA)
  if (length(unique(sample_groups)) >= 2 && nrow(pro_plot_df) > length(unique(sample_groups))) {
    aov_global <- aov(Residuals ~ Group, data = pro_plot_df)
    global_p <- summary(aov_global)[[1]][["Pr(>F)"]][1] 
    global_p_label <- ifelse(global_p < 0.001, "P < 0.001", sprintf("P = %.3f", global_p))
    
    # 2. 计算事后多重比较并生成显著性字母标记 (Tukey HSD) - 保持原有细节
    aov_posthoc <- agricolae::HSD.test(aov_global, "Group", group = TRUE)  
    
    # 提取带有字母的统计结果数据框
    sig_letters <- aov_posthoc$groups
    sig_letters$Group <- rownames(sig_letters) 
    
    # 3. 计算每个分组在图上的最大 Y 值，以便悬浮字母
    letter_pos <- pro_plot_df %>%
      group_by(Group) %>%
      summarise(Max_Y = max(Residuals, na.rm = TRUE)) %>%
      left_join(sig_letters, by = "Group")
    
    # 动态计算 Y 轴上限
    y_upper_limit <- max(letter_pos$Max_Y) * 1.15
    
    # 4. 绘制终极版带统计标记的箱线图 (Tukey 字母)
    p_res_box_sig <- ggplot(pro_plot_df, aes(x = Group, y = Residuals, fill = Group)) +
      geom_boxplot(alpha = 0.6, outlier.shape = NA, width = 0.8) +
      geom_jitter(aes(color = Group), width = 0.15, size = 2, alpha = 0.8) +
      labs(
        title = "Cross-Kingdom Concordance by Treatment",
        subtitle = paste0("One-way ANOVA: ", global_p_label), 
        x = "Treatment Group", 
        y = "Procrustes Residual"
      ) +
      geom_text(
        data = letter_pos, 
        aes(x = Group, y = Max_Y + (max(Max_Y) * 0.05), label = groups), 
        inherit.aes = FALSE, 
        size = 5.5, fontface = "bold", color = "black", vjust = 0
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.1)), 
        limits = c(0, y_upper_limit)
      ) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 11),
        plot.subtitle = element_text(face = "italic", color = "grey30", hjust = 0.5, size = 11),
        axis.title.x = element_text(face = "bold", color = "black", size = 11, margin = margin(t = 10)),
        axis.title.y = element_text(face = "bold", color = "black", size = 11, margin = margin(r = 10)),
        axis.text.x = element_text(color = "black", size = 11, face = "bold"),
        axis.text.y = element_text(color = "black", size = 11),
        legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
    
    ggsave(paste0(outDir_Procrustes, "/4_Procrustes_Residuals_Boxplot_", keyword, "_sig.pdf"), plot = p_res_box_sig, width = 3, height = 3)
    ggsave(paste0(outDir_Procrustes, "/4_Procrustes_Residuals_Boxplot_", keyword, "_sig.png"), plot = p_res_box_sig, width = 3, height = 3, dpi = 300)
    
  
  
  cat(sprintf("    ✅ [%s] 层级分析与可视化图谱导出完成！\n", toupper(keyword)))
}

cat("\n🎉 全部多维交叉界分析任务平稳结束！可以去输出目录查看了！\n")


} else {
  cat("⚠️ 警告：有效组别不足或样本量过少，跳过统计分析。\n")
}






#### Figure S10 ####


##### Figure S10A ######
rm(list = ls()) # 清空当前工作区环境变量
gc()            # 执行系统垃圾回收


setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")

# 建立输出目录
outDir <- "./2_analysis_R/2_Gene-path/1_alpha"


library(agricolae)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales) # 确保加载 scales 包

# --- 5.1 独立读取历史数据（实现解耦，方便单独调试本段绘图） ---
output_csv <- file.path(outDir, "All_Alpha_Diversity_Indices.csv")

if (file.exists(output_csv)) {
  cat("正在从本地读取 Alpha 多样性数据进行绘图调试...\n")
  alpha_results <- read.csv(output_csv, check.names = FALSE)
  
  # ⚠️ 关键：从 CSV 读取后必须重新定义 Group 的因子顺序，否则统计和绘图顺序会乱
  alpha_results$Group <- factor(alpha_results$Group, levels = c("Syn","CK",  "H18", "H20"))
} else {
  stop("❌ 未找到历史导出的 CSV 文件，请确保前 4 部分至少完整运行过一次！")
}

# --- 5.2 精简目标指标：仅保留指定的 4 个核心指标进行方差分析 ---
core_indices <- c("Observed", "Shannon", "Simpson", "Pielou")

# 创建一个空的列表来存储统计显著性字母
letter_list <- list()

for (idx in core_indices) {
  # 1. 拟合线性模型 (ANOVA)
  model <- aov(as.formula(paste(idx, "~ Group")), data = alpha_results)
  
  # 2. Tukey HSD 检验
  out <- HSD.test(model, "Group", group = TRUE)
  
  # 3. 提取字母
  letters_df <- out$groups
  letters_df$Group <- rownames(letters_df)
  
  # 4. 动态计算字母显示的 Y 轴高度
  stats_val <- alpha_results %>%
    group_by(Group) %>%
    summarise(
      max_val = max(!!sym(idx), na.rm = TRUE),
      total_range = max(alpha_results[[idx]], na.rm = TRUE) - min(alpha_results[[idx]], na.rm = TRUE)
    ) %>%
    mutate(
      y_pos = max_val + ifelse(total_range > 0, total_range * 0.15, max_val * 0.001)
    )
  
  # 5. 合并字母和高度信息
  plot_letters <- merge(letters_df, stats_val, by = "Group")
  plot_letters$Index <- idx
  
  # 提取公共列，确保结构一致
  plot_letters_clean <- plot_letters[, c("Group", "groups", "y_pos", "Index")]
  
  letter_list[[idx]] <- plot_letters_clean
}

# 合并所有指标的显著性标记表
all_letters <- do.call(rbind, letter_list)

# --- 5.3 转换 Alpha 结果为长表用于绘图 ---
alpha_long <- alpha_results %>%
  pivot_longer(cols = all_of(core_indices), names_to = "Index", values_to = "Value")

# --- 5.4 标签定制化：将 Simpson 修改为 Simpson（1-D） 并统一排序 ---
all_letters$Index <- as.character(all_letters$Index)
all_letters$Index[all_letters$Index == "Simpson"] <- "Simpson（1-D）"

alpha_long$Index <- as.character(alpha_long$Index)
alpha_long$Index[alpha_long$Index == "Simpson"] <- "Simpson（1-D）"

# 统一设定 4 个指标的因子顺序（完美适应 2x2 宫格布局）
plot_order <- c("Observed", "Shannon", "Simpson（1-D）", "Pielou")
all_letters$Index <- factor(all_letters$Index, levels = plot_order)
alpha_long$Index <- factor(alpha_long$Index, levels = plot_order)

# --- 5.5 定义智能 Y 轴标签函数 ---
smart_y_labels <- function(x) {
  x_clean <- x[!is.na(x)] 
  if(length(x_clean) == 0) return(x)
  
  if (max(abs(x_clean)) > 100) {
    return(scales::label_scientific(parse = TRUE)(x))
  } else {
    return(scales::label_number(drop0trailing = TRUE)(x))
  }
}

# --- 5.6 绘图与保存 (4个指标刚好构成 2x2 完美排版) ---
p_alpha_final <- ggplot(alpha_long, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA, width = 0.6) +
  geom_jitter(width = 0.15, size = 1.2, color = "black", alpha = 0.5) +
  facet_wrap(~ Index, scales = "free", ncol = 2) +
  
  # 添加字母标记
  geom_text(data = all_letters, 
            aes(x = Group, y = y_pos, label = groups), 
            vjust = 0, size = 3.5, fontface = "bold") +
  
  # CK, Syn, H18, H20 组别颜色映射
  scale_fill_manual(values = c("CK" = "#4DBBD5", "Syn" = "#00A087", "H18" = "#F39B7F", "H20" = "#DC0000")) +
  
  # 顶部留出 20% 空间给字母标记
  scale_y_continuous(
    expand = expansion(mult = c(0.1, 0.2)),
    labels = smart_y_labels
  ) + 
  
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "gray90"),
    axis.text = element_text(size = 10, color = "black"),
    
    # 【优化部分】设置 x 和 y 轴标签字体大小与加粗，并调整边距防止文字重叠
    axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)), # t=10 增加顶部边距
    axis.title.y = element_text(size = 10, face = "bold", margin = margin(r = 10)), # r=10 增加右侧边距
    
    panel.grid.minor = element_blank()
  )

# 打印查看效果
print(p_alpha_final)

# 保存高清晰度图片
ggsave(file.path(outDir, "Alpha_Diversity_Core_4Indices.pdf"), p_alpha_final, width = 5, height = 3)
ggsave(file.path(outDir, "Alpha_Diversity_Core_4Indices.png"), p_alpha_final, width = 5, height = 3, dpi = 300)

cat("✅ 绘图脚本调试完成！精简后的 4 核心指标汇总图已成功导出至输出目录。\n")








##### Figure S10B ######
rm(list = ls()) # 清空当前工作区环境变量
gc()            # 执行系统垃圾回收

# 加载 microeco
library(microeco)
library(magrittr)

library(ggplot2)

setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")

# 建立输出目录
outDir <- "./2_analysis_R/2_Gene-path/2_beta"
if(!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)


cat("正在读取并清洗数据...\n")
count_data <- read.table("./2_analysis_R/2_Gene-path/0_data_clean/1_Clean_bac_TPM.tsv", 
                         header=TRUE, row.names=1, sep="\t", check.names = FALSE)



# 净化列名
# colnames(count_data) <- gsub("_quant", "", colnames(count_data))

# 提前构建样本信息表 (Metadata) —— microeco 要求行名必须与 count_data 列名完全一致
sample_names <- colnames(count_data)
groups <- gsub("_.*", "", sample_names)

metadata <- data.frame(
  SampleID = sample_names,
  Group    = factor(groups, levels = c("Syn", "CK", "H18", "H20")) # 强制设定因子顺序
)
rownames(metadata) <- metadata$SampleID


cat("正在进行 Beta 多样性前的基因过滤与对象构建...\n")
count_data_filter <- count_data[rowSums(count_data > 1) >= 3, ]


dataset <- microtable$new(sample_table = metadata, otu_table = count_data_filter)


cat("正在计算 Bray-Curtis 距离矩阵...\n")
# 该步计算较为耗时，microeco 内部经过了优化
dataset$cal_betadiv(method = "bray")


cat("正在执行 PERMANOVA 差异检验...\n")

t1 <- trans_beta$new(dataset = dataset, group = "Group", measure = "bray")

tryCatch({
  t1$cal_permanova(permutations = 999)
}, error = function(e){
  cat("检测到 R6 方法调用异常，尝试备用方案...\n")
  # 备用：直接使用 vegan 包进行计算并手动注入结果
  library(vegan)
  adonis_res <- adonis2(as.dist(dataset$beta_diversity$bray) ~ Group, 
                        data = as.data.frame(dataset$sample_table), 
                        permutations = 999)
  t1$res_permanova <- adonis_res
})

# 导出结果
write.csv(t1$res_permanova, file.path(outDir, "PERMANOVA_test_results.csv"))

# 导出 PERMANOVA 结果表格
write.csv(t1$res_permanova, file.path(outDir, "PERMANOVA_test_results.csv"))
cat("✅ PERMANOVA 统计表格已导出。\n")




cat("正在进行 PCoA 分析...\n")
t1$cal_ordination(method = "PCoA")

my_colors <- c("Syn" = "#00A087", "CK" = "#4DBBD5", "H18" = "#F39B7F", "H20" = "#DC0000")




library(ggrepel)


t1$sample_table$SampleID <- rownames(t1$sample_table)

# 接下来正常运行你的绘图代码
p_pcoa_1 <- t1$plot_ordination(
  plot_color = "Group", 
  plot_shape = NULL, 
  # plot_type = c("point", "chull"), 
  plot_type = "point",
  ellipse_chull_fill = TRUE, 
  ellipse_chull_alpha = 0.1
) +
  # 现在可以放心地映射我们刚刚创建的 SampleID 列了
  geom_text_repel(
    aes(label = SampleID),     
    size = 2,                  
    show.legend = FALSE,       
    max.overlaps = 50,         
    box.padding = 0.4,         
    point.padding = 0.2,       
    segment.color = "grey50"   
  ) +
  
  scale_color_manual(values = my_colors) +
  scale_fill_manual(values = my_colors) +
  theme_bw() +
  
  # 标注 PERMANOVA 统计结果
  annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, 
           label = paste0("PERMANOVA: R2 = ", round(t1$res_permanova$R2[1], 3), 
                          "\nP = ", t1$res_permanova$`Pr(>F)`[1]),
           size = 3, fontface = "italic") +
  
  labs(title = "PCoA (Bray-Curtis distance)") +
  
  # 字体与排版精细化设置
  theme(
    axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 10, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 10, color = "black"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10, face = "bold")
  )

# 打印查看效果
print(p_pcoa_1)

# 保存 PCoA 图片
ggsave(file.path(outDir, "Beta_PCoA_Plot-1.pdf"), p_pcoa_1, width = 3.5, height = 2.5)
ggsave(file.path(outDir, "Beta_PCoA_Plot-1.png"), p_pcoa_1, width = 3.5, height = 2.5, dpi = 300)





##### Figure S11 ######



rm(list = ls()) 
gc()            


setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")

outDir <- "./2_analysis_R/2_Gene-path/4_EGGnog/5_KEGGPathway_Module_KO-DE"
if(!dir.exists(outDir)) dir.create(outDir, recursive = TRUE)

# 加载依赖包
suppressWarnings(suppressMessages({
  library(ggplot2)
  library(dplyr)
  library(RColorBrewer)
  library(reshape2)
  library(DESeq2) 
  library(ggVennDiagram) 
  library(ggrepel)       
  library(clusterProfiler)
  library(enrichplot)    
  library(ReporterScore) 
  library(pheatmap)      
  library(tibble)
  library(UpSetR)         
  library(KEGGREST) 
  library(stringr)
}))

# 设定全局差异筛选阈值
padj_cutoff <- 0.05
logFC_cutoff <- 1      


Category <- c("KEGG_ko")

for (i in 1:length(Category)){
  
  keyword <- Category[i]
  cat(paste0("\n>>> 正在处理分类: ", keyword, " <<<\n"))
  
  cat_outDir <- file.path(outDir, keyword)
  if(!dir.exists(cat_outDir)) dir.create(cat_outDir, recursive = TRUE)
  

  cat("  - 正在通过 KEGGREST 获取官方最新描述字典...\n")
  kegg_dict <- c()
  tryCatch({
    if (keyword == "KEGG_ko") {
      tmp <- keggList("ko")
      names(tmp) <- gsub("^(ko:)?(K|k)", "K", names(tmp)) 
      kegg_dict <- tmp
    } else if (keyword == "KEGG_Pathway") {
      tmp <- keggList("pathway")
      names(tmp) <- gsub("^(path:)?(map|ko)", "ko", names(tmp)) 
      kegg_dict <- tmp
    } else if (keyword == "KEGG_Module") {
      tmp <- keggList("module")
      names(tmp) <- gsub("^(md:)?(M|m)", "M", names(tmp)) 
      kegg_dict <- tmp
    }
    cat("    ✅ 官方字典下载并清洗成功！\n")
  }, error = function(e) {
    cat("    ⚠️ 警告：获取 KEGG 官方在线字典失败，将使用原始 ID 代替功能描述。\n")
  })
  
  # 定义更精简的标签映射函数
  get_feature_label <- function(ids) {
    if (length(kegg_dict) == 0) return(ids)
    res <- character(length(ids))
    for (j in seq_along(ids)) {
      if (ids[j] == "Others") {
        res[j] <- "Others"
      } else {
        desc <- kegg_dict[ids[j]]
        if (is.na(desc)) desc <- "Unknown"
        
        desc <- gsub(" - .*", "", desc)
        desc <- gsub(" \\[BR:.*\\]", "", desc)
        
        if (nchar(desc) > 30) desc <- paste0(substr(desc, 1, 27), "...")
        res[j] <- paste0(ids[j], ": ", desc)
      }
    }
    return(res)
  }
  
  count_data <- read.table(paste0("./2_analysis_R/2_Gene-path/4_EGGnog/1_Category_abundance/", keyword ,"_Counts.txt"), header = TRUE, sep = "\t", check.names = FALSE)
  
  rownames(count_data) <- count_data[[1]]
  count_mat <- round(as.matrix(count_data[, -1]))
  count_mat <- count_mat[rowSums(count_mat) > 0, ]
  
  sample_names <- colnames(count_mat)
  groups <- gsub("_[0-9]+$", "", sample_names) 
  
  metadata <- data.frame(
    SampleID = sample_names,
    Group    = factor(groups, levels = c("Syn","CK",  "H18", "H20")) 
  )
  rownames(metadata) <- metadata$SampleID
  

  dds <- DESeqDataSetFromMatrix(countData = count_mat, colData = metadata, design = ~ Group)
  dds$Group <- relevel(dds$Group, ref = "Syn")
  dds <- DESeq(dds)
  norm_counts <- counts(dds, normalized = TRUE)
  
  compare_groups <- c("CK", "H18", "H20")
  up_list <- list()
  down_list <- list()
  
  if (keyword == "KEGG_ko") {
    cat("  * 加载 KO TPM 数据以备 Reporter Score 分析使用...\n")
    ko_tpm_for_rs <- read.table("./2_analysis_R/2_Gene-path/4_EGGnog/1_Category_abundance/KEGG_ko_TPM.txt", 
                                header = TRUE, row.names = 1, sep = "\t", check.names = FALSE)
    ko_universe <- gsub("^ko:", "", rownames(count_mat)) 
  }
  

  for (comp in compare_groups) {
    comp_name <- paste0(comp, "_vs_Syn")
    cat(paste0("  - 正在分析比较组: ", comp_name, "\n"))
    
    res <- results(dds, contrast = c("Group", comp, "Syn"))
    res_df <- as.data.frame(res)
    res_df$Feature <- rownames(res_df)
    
    res_df$padj[is.na(res_df$padj)] <- 1
    res_df$log2FoldChange[is.na(res_df$log2FoldChange)] <- 0
    
    res_df$Significance <- "NS"
    res_df$Significance[res_df$padj < padj_cutoff & res_df$log2FoldChange >= logFC_cutoff] <- "Up"
    res_df$Significance[res_df$padj < padj_cutoff & res_df$log2FoldChange <= -logFC_cutoff] <- "Down"
    res_df$Significance <- factor(res_df$Significance, levels = c("Up", "Down", "NS"))
    
    up_count <- sum(res_df$Significance == "Up")
    down_count <- sum(res_df$Significance == "Down")
    
    up_list[[comp_name]] <- res_df$Feature[res_df$Significance == "Up"]
    down_list[[comp_name]] <- res_df$Feature[res_df$Significance == "Down"]
    
    res_df$Description <- "Unknown function"
    if (length(kegg_dict) > 0) {
      clean_feat_ids <- gsub("^ko:", "", res_df$Feature)
      mapped_descs <- kegg_dict[clean_feat_ids]
      mapped_descs[is.na(mapped_descs)] <- "Unknown function"
      res_df$Description <- gsub(" - .*", "", mapped_descs)
    }
    
    res_export <- res_df[, c("Feature", "Description", "baseMean", "log2FoldChange", "lfcSE", "stat", "pvalue", "padj", "Significance")]
    write.table(res_export, file = file.path(cat_outDir, paste0(keyword, "_", comp_name, "_All.txt")), sep = "\t", quote = FALSE, row.names = FALSE)
    
    sig_df <- res_export %>% filter(Significance %in% c("Up", "Down"))
    write.table(sig_df, file = file.path(cat_outDir, paste0(keyword, "_", comp_name, "_SigOnly.txt")), sep = "\t", quote = FALSE, row.names = FALSE)
    
    

    cat("    -> 正在运行 Reporter Score 分析...\n")
    
    # 1. 过滤并提取当前特定的比较组(comp)与目标组(Syn)的样本元数据
    sub_meta <- metadata[metadata$Group %in% c("Syn", comp), ]
    sub_meta$Group <- factor(sub_meta$Group, levels = c(comp, "Syn"))
    sub_tpm  <- ko_tpm_for_rs[, rownames(sub_meta), drop = FALSE]
    
    # 2. 清洗表达矩阵行名：剥离合并可能存在的逗号分隔同名KO，并移除 "ko:" 前缀
    clean_kos <- sapply(strsplit(rownames(sub_tpm), ","), `[`, 1)
    rownames(sub_tpm) <- gsub("^ko:", "", clean_kos)
    sub_tpm <- sub_tpm[rowSums(sub_tpm) > 0, ] # 剔除全零表达的行
    
    # 3. 按相同 KO 编号对丰度矩阵进行求和合并，转化为标准分析格式
    sub_tpm <- as.data.frame(sub_tpm) %>% 
      rownames_to_column("KO") %>% 
      group_by(KO) %>% 
      summarise(across(everything(), sum)) %>% 
      column_to_rownames("KO")
    

    set.seed(123) # 固定随机数种子，确保置换检验结果可重复
    tryCatch({
      # 调用基础分析函数计算 Reporter Score (基于 t.test 差异算法)
      rs_pathway <- reporter_score(sub_tpm, metadata = sub_meta, group = "Group", method = "t.test", type = "pathway")
      res_df_pw  <- if ("reporter_s" %in% names(rs_pathway)) rs_pathway$reporter_s else if ("result" %in% names(rs_pathway)) rs_pathway$result else tryCatch(as.data.frame(rs_pathway), error=function(e) NULL)
      
      if (!is.null(res_df_pw) && is.data.frame(res_df_pw) && nrow(res_df_pw) > 0) {
        
        # 【步骤A：基础异常清洗】剔除 ID 缺失、描述缺失、或者值为字符串 "NA" 的幽灵行
        res_df_pw <- res_df_pw[!is.na(res_df_pw[[1]]) & !is.na(res_df_pw$Description) & as.character(res_df_pw$Description) != "NA", ]
        
        # 将最原始、未经任何高级过滤的完整分析表导出备份
        write.table(res_df_pw, file = file.path(cat_outDir, paste0("ReporterScore_Pathway_All_", comp_name, ".txt")), sep = "\t", quote = FALSE, row.names = FALSE)
        
        # 动态捕捉包含得分信息的列名（兼容不同插件输出的 ReportScore/Score/RS 等列名）
        score_col <- grep("eport|score|RS", colnames(res_df_pw), value = TRUE, ignore.case = TRUE)[1]
        
        if (!is.na(score_col)) {
  
          sig_df_pw <- res_df_pw[which(abs(res_df_pw[[score_col]]) >= 1.65 & 
                                         res_df_pw$Significant_K_num >= 2 & 
                                         res_df_pw$Exist_K_num >= 5), ]

          if (nrow(sig_df_pw) > 0) {
            noise_keywords <- paste0("disease|cancer|infection|addiction|Alzheimer|",
                                     "Huntington|Parkinson|cardiomyopathy|meiosis|",
                                     "Antigen|synapse|potentiation|immune network|",
                                     "cell lineage|synaptic vesicle")
            sig_df_pw <- sig_df_pw[!grepl(noise_keywords, sig_df_pw$Description, ignore.case = TRUE), ]
          }
          
          # 确保通过高级质控和生物学清洗后，依然有有效通路存在
          if (nrow(sig_df_pw) > 0) {
            
            # 判定富集方向 (RS > 0 判定为上调 Up，反之判定为下调 Down)
            sig_df_pw$Direction <- ifelse(sig_df_pw[[score_col]] > 0, "Up", "Down")
            
   
            sig_df_all_export <- sig_df_pw[order(abs(sig_df_pw[[score_col]]), decreasing = TRUE), ]
            write.table(sig_df_all_export, file = file.path(cat_outDir, paste0("ReporterScore_Pathway_SigFiltered_", comp_name, ".txt")), sep = "\t", quote = FALSE, row.names = FALSE)
            

            df_up   <- sig_df_pw %>% filter(Direction == "Up") %>% arrange(desc(abs(.data[[score_col]]))) %>% head(15)
            df_down <- sig_df_pw %>% filter(Direction == "Down") %>% arrange(desc(abs(.data[[score_col]]))) %>% head(15)
            
            # 合并并重新按照分数整体降序排列，准备用于画图
            sig_df_pw <- rbind(df_up, df_down)
            sig_df_pw <- sig_df_pw[order(sig_df_pw[[score_col]], decreasing = TRUE), ]

            
            # 新增 Plot_Label 列用于画图（包含换行），不污染原 Description 列，防止文本超界
            sig_df_pw$Plot_Label <- str_wrap(as.character(sig_df_pw$Description), width = 55)
            sig_df_pw$Plot_Label <- factor(sig_df_pw$Plot_Label, levels = rev(unique(sig_df_pw$Plot_Label)))
            
            # 导出最终用于绘图的融合数据（仅含入选画图的 Top 通路）
            write.table(sig_df_pw, file = file.path(cat_outDir, paste0("ReporterScore_Pathway_Bar_PlotData_", comp_name, ".txt")), sep = "\t", quote = FALSE, row.names = FALSE)
            
            # 动态计算出图的高度 (每个特征预留 0.25 的高度，保底4.5)，避免条目拥挤
            plot_height <- max(4.5, nrow(sig_df_pw) * 0.1 + 1)
            
            # 绘图映射改为 Plot_Label
            p_bar <- ggplot(sig_df_pw, aes(x = Plot_Label, y = .data[[score_col]], fill = Direction)) +
              geom_bar(stat = "identity", color = "black", linewidth = 0.2, width = 0.7) + 
              coord_flip() + 
              scale_fill_manual(values = c("Up" = "#D73027", "Down" = "#4575B4")) +
              geom_hline(yintercept = c(-1.65, 1.65), linetype = "dashed", color = "gray50") + 
              theme_bw() + 
              labs(title = paste0("Reporter Score (Pathway): ", comp_name), x = "", y = "Reporter Score") +
              theme(legend.position = "none", 
                    axis.text.y = element_text(color = "black", size = 11.5),
                    axis.text.x = element_text(color = "black", size = 11.5))
            
            # 输出时应用动态高度 plot_height
            ggsave(file.path(cat_outDir, paste0("ReporterScore_Pathway_Bar_", comp_name, ".pdf")), plot = p_bar, width = 7, height = plot_height)
            ggsave(file.path(cat_outDir, paste0("ReporterScore_Pathway_Bar_", comp_name, ".png")), plot = p_bar, width = 7, height = plot_height, dpi = 300)
          }
        }
      }
    }, error = function(e) { cat("    * Reporter Score (Pathway) 跳过.\n") })
    
  } 




##### Figure S12 ######


  rm(list = ls()) 
  gc() 
  
  suppressMessages({
    if (!requireNamespace("multcompView", quietly = TRUE)) install.packages("multcompView")
    if (!requireNamespace("DescTools", quietly = TRUE)) install.packages("DescTools")
    
    library(dplyr)
    library(tidyr)
    library(stringr)
    library(ggplot2)
    library(RColorBrewer)
    library(multcompView)
    library(DescTools)
  })
  
  # 设置全局路径 (请根据实际情况确认工作目录)
  setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")
  
  outDir <- "./2_analysis_R/2_Gene-path/4_EggNog/2_QS_Pipeline_from_DEGs"
  dir_matrices <- file.path(outDir, "1_Abundance_Matrices")
  

  cat("📥 正在读取丰度矩阵并重建样本元数据...\n")
  
  tpm_system_df <- read.table(file.path(dir_matrices, "QS_SystemType_TPM.tsv"), header=TRUE, sep="\t", check.names=FALSE)
  tpm_pure_func_df <- read.table(file.path(dir_matrices, "QS_PureFunction_TPM_for_Procrustes.tsv"), header=TRUE, sep="\t", check.names=FALSE)
  
  # 动态提取样本名并构建分组 (排除非样本列)
  sample_names <- setdiff(colnames(tpm_system_df), "QS_System_Type")
  groups <- gsub("_[0-9]+$", "", sample_names)
  
  # 设定统一顺序基准
  target_levels <- c("Syn", "CK", "H18", "H20")
  
  # 重构全局 Metadata
  global_annotation_col <- data.frame(Group = factor(groups, levels = target_levels))
  rownames(global_annotation_col) <- sample_names
  global_ann_colors <- list(Group = c("Syn" = "grey50", "CK" = "#00AFBB", "H18" = "#E7B800", "H20" = "#FC4E07"))

  cat("🎨 [1/2] 正在处理 QS_System_Type 图表与差异分析...\n")
  dir_plots1 <- file.path(outDir, "2_QS_SystemType_Plots")
  dir.create(dir_plots1, showWarnings = FALSE)
  
  # 定义通用绘图保存函数 (第四阶段)
  save_dual_plot_sys <- function(p, file_path_no_ext, w=8, h=5) {
    ggsave(paste0(file_path_no_ext, ".pdf"), plot=p, width=w, height=h)
    ggsave(paste0(file_path_no_ext, ".png"), plot=p, width=w, height=h, dpi=300)
  }
  
  # 1. 宽表转长表
  tpm_sys_long <- tpm_system_df %>% 
    tidyr::pivot_longer(cols = -QS_System_Type, names_to = "Sample", values_to = "TPM") %>%
    dplyr::left_join(global_annotation_col %>% tibble::rownames_to_column("Sample"), by = "Sample")
  
  sys_colors <- brewer.pal(max(3, length(unique(tpm_sys_long$QS_System_Type))), "Set2")
  names(sys_colors) <- unique(tpm_sys_long$QS_System_Type)
  
  
  # 3. 数据聚合 (基础均值和误差)
  sys_summary <- tpm_sys_long %>% dplyr::group_by(QS_System_Type, Group) %>%
    dplyr::summarise(Mean_TPM = mean(TPM), SE = sd(TPM)/sqrt(n()), .groups = "drop")
  y_offset_sys <- max(sys_summary$Mean_TPM + sys_summary$SE, na.rm = TRUE) * 0.05
  
 
  # --- 3.2 Dunnett's Test 计算、绘图与结果导出 ---
  cat("  -> 正在计算 Dunnett's Test (对照组 Syn) 并生成星号...\n")
  dunnett_sig_df <- data.frame()
  syn_means <- sys_summary %>% dplyr::filter(Group == "Syn") %>% dplyr::select(QS_System_Type, Syn_Mean = Mean_TPM)
  
  for (sys in unique(tpm_sys_long$QS_System_Type)) {
    sub_dat <- tpm_sys_long %>% dplyr::filter(QS_System_Type == sys)
    
    if ("Syn" %in% levels(sub_dat$Group)) {
      dun_res <- DunnettTest(TPM ~ Group, data = sub_dat, control = "Syn")
      pval_mat <- dun_res$Syn
      if(!is.null(pval_mat)) {
        for(rn in rownames(pval_mat)) {
          grp <- strsplit(rn, "-")[[1]][1]
          dunnett_sig_df <- rbind(dunnett_sig_df, data.frame(QS_System_Type = sys, Group = grp, p_val = pval_mat[rn, "pval"]))
        }
      }
    }
  }
  
  if(nrow(dunnett_sig_df) > 0) {
    dunnett_sig_df <- dunnett_sig_df %>%
      dplyr::mutate(signif = dplyr::case_when(p_val < 0.001 ~ "***", p_val < 0.01 ~ "**", p_val < 0.05 ~ "*", TRUE ~ ""))
  }
  
  sys_summary_dunnett <- sys_summary %>%
    dplyr::left_join(dunnett_sig_df, by = c("QS_System_Type", "Group")) %>%
    dplyr::left_join(syn_means, by = "QS_System_Type") %>%
    dplyr::mutate(
      signif = tidyr::replace_na(signif, ""),
      Direction = dplyr::case_when(
        signif == "" ~ "None",
        Mean_TPM > Syn_Mean ~ "Up",
        Mean_TPM < Syn_Mean ~ "Down",
        TRUE ~ "None"
      )
    )
  
  # 🌟【核心修复点 2】：锁定 Dunnett 的数据框因子顺序
  sys_summary_dunnett$Group <- factor(sys_summary_dunnett$Group, levels = target_levels)
  
  p_bar_dunnett_sys <- ggplot(sys_summary_dunnett, aes(x = QS_System_Type, y = Mean_TPM, fill = Group, group = Group)) +
    geom_col(position = position_dodge(width = dodge_width), 
             width = bar_width, color = "black", linewidth = 0.3) +
    geom_errorbar(aes(ymin = Mean_TPM, ymax = Mean_TPM + SE), 
                  position = position_dodge(width = dodge_width), 
                  width = 0.25, linewidth = 0.5) +
    geom_text(aes(y = Mean_TPM + SE + y_offset_sys, label = signif, color = Direction, group = Group), 
              position = position_dodge(width = dodge_width), 
              vjust = 0, size = 5, fontface = "bold") + 
    scale_color_manual(values = c("Up" = "#E64B35", "Down" = "#4DBBD5", "None" = "transparent"), guide = "none") +
    scale_fill_manual(values = global_ann_colors$Group) + 
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme_bw() +
    labs(title = "Mean TPM Abundance by Group (vs Syn, Dunnett's Test)", 
         x = "QS System Type", y = "Mean TPM") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, margin = margin(b = 15)),
      axis.title.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 12)),
      axis.title.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 12)),
      axis.text.x = element_text(size = 11, face = "bold", color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11, face = "bold", color = "black"),
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      legend.position = "top"
    )
  
  print(p_bar_dunnett_sys)
  save_dual_plot_sys(p_bar_dunnett_sys, file.path(dir_plots1, "QS_SystemType_Barplot_Dunnett"))
  
  write.csv(sys_summary_dunnett, file.path(dir_plots1, "QS_SystemType_Dunnett_Test_Results.csv"), row.names = FALSE)
  cat("✅ 第一层级差异分析完毕，已导出 Dunnett 结果表。\n\n")
  
  

  cat("🎨 [2/2] 正在处理 QS_Pure_Function 图表与差异分析...\n")
  dir_plots2 <- file.path(outDir, "2_QS_SystemFUN_Plots")
  dir.create(dir_plots2, showWarnings = FALSE)
  
  # 定义通用绘图保存函数
  save_dual_plot_func <- function(p, file_path_no_ext, w=8, h=7) {
    ggsave(paste0(file_path_no_ext, ".pdf"), plot=p, width=w, height=h)
    ggsave(paste0(file_path_no_ext, ".png"), plot=p, width=w, height=h, dpi=300)
  }
  
  # 1. 宽表转长表
  tpm_purefunc_long <- tpm_pure_func_df %>% 
    tidyr::pivot_longer(cols = -Pure_Function, names_to = "Sample", values_to = "TPM") %>%
    dplyr::left_join(global_annotation_col %>% tibble::rownames_to_column("Sample"), by = "Sample")
  
  # 动态插值生成充足的调色板
  nb_colors <- length(unique(tpm_purefunc_long$Pure_Function))
  purefunc_colors <- colorRampPalette(brewer.pal(8, "Set2"))(nb_colors)
  names(purefunc_colors) <- unique(tpm_purefunc_long$Pure_Function)
  
  # 2. 堆叠柱形图
  p_stack_func <- ggplot(tpm_purefunc_long, aes(x = Sample, y = TPM, fill = Pure_Function)) +
    geom_col(position = "stack", width = 0.8) + scale_fill_manual(values = purefunc_colors) + 
    theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_dual_plot_func(p_stack_func, file.path(dir_plots2, "QS_PureFunction_Stacked"))
  
  # 3. 数据聚合 (基础均值和误差)
  purefunc_summary <- tpm_purefunc_long %>% dplyr::group_by(Pure_Function, Group) %>%
    dplyr::summarise(Mean_TPM = mean(TPM), SE = sd(TPM)/sqrt(n()), .groups = "drop")
  y_offset_func <- max(purefunc_summary$Mean_TPM + purefunc_summary$SE, na.rm = TRUE) * 0.05
  
 
  # --- 3.2 Dunnett's Test 计算、绘图与结果导出 ---
  cat("  -> 正在计算 Dunnett's Test (对照组 Syn) 并生成星号...\n")
  dunnett_sig_func_df <- data.frame()
  syn_means_func <- purefunc_summary %>% dplyr::filter(Group == "Syn") %>% dplyr::select(Pure_Function, Syn_Mean = Mean_TPM)
  
  for (func in unique(tpm_purefunc_long$Pure_Function)) {
    sub_dat <- tpm_purefunc_long %>% dplyr::filter(Pure_Function == func)
    
    if ("Syn" %in% levels(sub_dat$Group)) {
      dun_res <- DunnettTest(TPM ~ Group, data = sub_dat, control = "Syn")
      pval_mat <- dun_res$Syn
      if(!is.null(pval_mat)) {
        for(rn in rownames(pval_mat)) {
          grp <- strsplit(rn, "-")[[1]][1]
          dunnett_sig_func_df <- rbind(dunnett_sig_func_df, data.frame(Pure_Function = func, Group = grp, p_val = pval_mat[rn, "pval"]))
        }
      }
    }
  }
  
  if(nrow(dunnett_sig_func_df) > 0) {
    dunnett_sig_func_df <- dunnett_sig_func_df %>%
      dplyr::mutate(signif = dplyr::case_when(p_val < 0.001 ~ "***", p_val < 0.01 ~ "**", p_val < 0.05 ~ "*", TRUE ~ ""))
  }
  
  purefunc_summary_dunnett <- purefunc_summary %>%
    dplyr::left_join(dunnett_sig_func_df, by = c("Pure_Function", "Group")) %>%
    dplyr::left_join(syn_means_func, by = "Pure_Function") %>%
    dplyr::mutate(
      signif = tidyr::replace_na(signif, ""),
      Direction = dplyr::case_when(
        signif == "" ~ "None",
        Mean_TPM > Syn_Mean ~ "Up",
        Mean_TPM < Syn_Mean ~ "Down",
        TRUE ~ "None"
      )
    )
  
  # 🌟【核心修复点 4】：强制设定第五阶段的 Dunnett 图表组别因子
  purefunc_summary_dunnett$Group <- factor(purefunc_summary_dunnett$Group, levels = target_levels)
  
  p_bar_dunnett_func <- ggplot(purefunc_summary_dunnett, aes(x = Pure_Function, y = Mean_TPM, fill = Group, group = Group)) +
    geom_col(position = position_dodge(width = dodge_width), 
             width = bar_width, color = "black", linewidth = 0.3) +
    geom_errorbar(aes(ymin = Mean_TPM, ymax = Mean_TPM + SE), 
                  position = position_dodge(width = dodge_width), 
                  width = 0.25, linewidth = 0.5) +
    
    # --- 完美贴合的显著性星号层 ---
    geom_text(aes(y = Mean_TPM + SE, label = signif, color = Direction, group = Group), 
              position = position_dodge(width = dodge_width), 
              vjust = -0.5, # 🌟 修正：将其改回 -0.5，真正实现向上偏移，防止星号与误差线重叠
              size = 4, fontface = "bold") +
    
    scale_color_manual(values = c("Up" = "#E64B35", "Down" = "#4DBBD5", "None" = "transparent"), guide = "none") +
    scale_fill_manual(values = global_ann_colors$Group) + 
    

  scale_y_continuous(
    limits = c(0, 13500), 
    expand = expansion(mult = c(0, 0.1))
  ) +
    
    # Y 轴截断设置 (ggbreak)
    scale_y_break(c(6500, 11300), space = 0.1, scales = 1) +
    
    theme_bw() +
    labs(title = "Mean TPM Abundance by Group (vs Syn, Dunnett's Test)", 
         x = "QS Pure Function", y = "Mean TPM") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, margin = margin(b = 15)),
      axis.title.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 12)),
      axis.title.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 12)),
      axis.text.x = element_text(size = 11, face = "bold", color = "black", angle = 45, hjust = 1),
      axis.text.y = element_text(size = 11, face = "bold", color = "black"),
      legend.title = element_text(face = "bold", size = 11),
      legend.text = element_text(size = 10),
      legend.position = "top"
    )
  
  print(p_bar_dunnett_func)
  
  
  save_dual_plot_func(p_bar_dunnett_func, file.path(dir_plots2, "QS_Pure_Function_Barplot_Dunnett"))
  
  
  write.csv(purefunc_summary_dunnett, file.path(dir_plots2, "QS_PureFunction_Dunnett_Test_Results.csv"), row.names = FALSE)
  cat("✅ 第二层级差异分析完毕，已导出 Dunnett 结果表。\n")
  
  
  
  #### Figure S13 ####
  rm(list = ls()) 
  gc()        
  

  suppressMessages({
    library(data.table)  # 提供极速的大数据表读写与原生内存级操作
    library(DescTools)   # 备用：非参数多重比较
    library(vegan)       # 提供生态学系统级 Beta 多样性 (Bray-Curtis) 计算
    library(KEGGREST)    # 提供在线/离线提取 KEGG 通路/模块字典的接口
  })
  
  # [路径规范] 统一设定工作目录与结果输出域
  setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")
  funcunt_Dir <- "./2_analysis_R/2_Gene-path/11_Fun-reduncance_Macro_Micro-4"
  if(!dir.exists(funcunt_Dir)) dir.create(funcunt_Dir, recursive = TRUE)
  
  # 🌟【核心新增】：全局声明固定的数据库出图顺序
  db_order <- c( "KEGG_Pathway","CAZy", "BGC_Type","NCYC_Pathway", "PCYC_Pathway", "SCYC_Pathway", "Fe",  "QS")
  
  
  cat("\n🌐 正在跨数据库拼接... 渲染全局大整合景观视图 (Global Summaries)...\n")
  out_global_dir <- file.path(funcunt_Dir, "Global_Cross_Database_Summary")
  if(!dir.exists(out_global_dir)) dir.create(out_global_dir, recursive = TRUE)
  
  # [图 6A] 全局系统冗余基线对比 (Global CWM-FR Profile)
  if(length(global_cwm_list) > 0) {
    global_cwm <- rbindlist(global_cwm_list)
    global_cwm$Group <- factor(global_cwm$Group, levels = target_levels)
    
    # 🌟【修改】：使用 rev() 反转全局固定顺序，以适配 coord_flip()，让 Fe 排在最上面
    global_cwm$Database <- factor(global_cwm$Database, levels = rev(db_order))
    

    cwm_sig_list <- list()
    for(db in unique(global_cwm$Database)) {
      sub_df <- global_cwm[global_cwm$Database == db, ]
      syn_vals <- sub_df$CWM_FR[sub_df$Group == "Syn"]
      
      for(g in unique(sub_df$Group)) {
        g_vals <- sub_df$CWM_FR[sub_df$Group == g]
        y_max <- max(g_vals, na.rm = TRUE) # 获取该组数据的最大值，作为标注的基准高度
        
        if(g == "Syn") {
          cwm_sig_list[[length(cwm_sig_list)+1]] <- data.frame(Database=db, Group=g, Mark="", Y_Max=y_max)
        } else {
          if(length(g_vals) >= 2 && length(syn_vals) >= 2 && sd(c(g_vals, syn_vals), na.rm=T) > 1e-8) {
            pval <- t.test(g_vals, syn_vals, var.equal=FALSE)$p.value
            mark <- ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "ns")))
          } else {
            mark <- "ns"
          }
          cwm_sig_list[[length(cwm_sig_list)+1]] <- data.frame(Database=db, Group=g, Mark=mark, Y_Max=y_max)
        }
      }
    }
    sig_df <- rbindlist(cwm_sig_list)
    # 必须严格同步因子水平，否则 position_dodge 会错乱！
    sig_df$Group <- factor(sig_df$Group, levels = target_levels)
    # 🌟【修改】：同步反转显著性数据框的 Database 因子顺序
    sig_df$Database <- factor(sig_df$Database, levels = rev(db_order))
    
    # 计算一个向右侧（由于翻转，实际是Y轴的上方）的微小偏移量，防止文本与胡须线重叠
    y_offset <- (max(global_cwm$CWM_FR, na.rm=T) - min(global_cwm$CWM_FR, na.rm=T)) * 0.03

    
    p_global_cwm <- ggplot(global_cwm, aes(x = Database, y = CWM_FR, fill = Group)) +
      geom_boxplot(alpha = 0.8, outlier.shape = NA, position = position_dodge(0.75), width=0.6) +
      
      # --- 新增文本图层：精准贴合显著性星号 ---
      geom_text(
        data = sig_df, 
        aes(y = Y_Max + y_offset, label = Mark), 
        position = position_dodge(width = 0.75), # 这里的 width 必须和 boxplot 保持 0.75 一致！
        hjust = 0, # coord_flip后，hjust=0 会让文本向右延伸
        vjust = 0.7, # 微调垂直居中对齐
        size = 5, fontface = "bold", color = "#E74C3C"
      ) +

    
    scale_fill_manual(values = group_colors) + 
      
      # --- 扩大 Y 轴留白范围，防止星号超出画布被裁剪 ---
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + 
      
      theme_bw() + 
      coord_flip() + 
      labs(title = "Global Baseline: System CWM-FR Across Databases", y = "CWM Functional Redundancy (CWM-FR)", x = "Functional Databases") +
      theme(axis.text=element_text(size=11, face="bold", color="black"), axis.title=element_text(size=13, face="bold"), legend.position="bottom")
    
    write.csv(global_cwm, file.path(out_global_dir, "PlotData_6A_Global_CWM_FR.csv"), row.names = FALSE)
    # 也将显著性结果保存下来，方便写文章时查阅
    write.csv(sig_df, file.path(out_global_dir, "PlotData_6A_Global_CWM_FR_Significance.csv"), row.names = FALSE)
    
    ggsave(file.path(out_global_dir, "6A_Plot_Global_CWM_FR_Profile.pdf"), plot=p_global_cwm, width=8, height=5)
    ggsave(file.path(out_global_dir, "6A_Plot_Global_CWM_FR_Profile.png"), plot=p_global_cwm, width=8, height=5, dpi=300, bg="white")
  }
  
  
  
  
 
  
  cat("\n🌐 正在跨数据库拼接... 渲染全局大整合景观视图 (Global Summaries)...\n")
  out_global_dir <- file.path(funcunt_Dir, "Global_Cross_Database_Summary")
  if(!dir.exists(out_global_dir)) dir.create(out_global_dir, recursive = TRUE)
  
  if(length(global_cwm_list) > 0) {
    global_cwm <- rbindlist(global_cwm_list)
    global_cwm$Group <- factor(global_cwm$Group, levels = target_levels)
    
    # 🌟【修改】：应用全局固定顺序
    global_cwm$Database <- factor(global_cwm$Database, levels = db_order)
    

    cwm_sig_list <- list()
    for(db in unique(global_cwm$Database)) {
      sub_df <- global_cwm[global_cwm$Database == db, ]
      syn_vals <- sub_df$CWM_FR[sub_df$Group == "Syn"]
      
      for(g in unique(sub_df$Group)) {
        g_vals <- sub_df$CWM_FR[sub_df$Group == g]
        y_max <- max(g_vals, na.rm = TRUE)
        
        if(g == "Syn") {
          cwm_sig_list[[length(cwm_sig_list)+1]] <- data.frame(Database=db, Group=g, Mark="", Y_Max=y_max)
        } else {
          if(length(g_vals) >= 2 && length(syn_vals) >= 2 && sd(c(g_vals, syn_vals), na.rm=T) > 1e-8) {
            pval <- t.test(g_vals, syn_vals, var.equal=FALSE)$p.value
            mark <- ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "ns")))
          } else {
            mark <- "ns"
          }
          cwm_sig_list[[length(cwm_sig_list)+1]] <- data.frame(Database=db, Group=g, Mark=mark, Y_Max=y_max)
        }
      }
    }
    sig_df <- rbindlist(cwm_sig_list)
    sig_df$Group <- factor(sig_df$Group, levels = target_levels)
    sig_df$Database <- factor(sig_df$Database, levels = db_order)
    
    # 向上偏移量（垂直图表）
    y_offset <- (max(global_cwm$CWM_FR, na.rm=T) - min(global_cwm$CWM_FR, na.rm=T)) * 0.03
      dodge_width <- 0.9 # 决定 4 个箱子作为一个整体占用的总宽度。调小此值，可拉开不同 Database 之间的间距。
    box_width <- 0.8    # 决定单个物理箱子的宽度。此值越小于 dodge_width，组内 4 个箱子之间的缝隙就越大。
    
    p_global_cwm <- ggplot(global_cwm, aes(x = Database, y = CWM_FR, fill = Group)) +
      geom_boxplot(alpha = 0.9, outlier.shape = NA, 
                   position = position_dodge(width = dodge_width), # 使用统一变量
                   width = box_width) +                            # 使用统一变量
      
     
      geom_text(
        data = sig_df, 
        aes(y = Y_Max + y_offset, label = Mark), 
        position = position_dodge(width = dodge_width), # 🌟【修复】：必须严格等于上面箱线图的 dodge_width！
        hjust = 0.5, # 垂直图表中：星号水平居中对齐到箱子
        vjust = 0,   # 垂直图表中：星号贴在 Y_Max 上方
        size = 3.5, fontface = "bold", color = "#E74C3C" # 稍微调大了 size 增加清晰度
      ) +
      
      scale_fill_manual(values = group_colors) + 
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) + 
      theme_bw() + 
      
      labs(title = "Global Baseline: System CWM-FR Across Databases", y = "CWM Functional Redundancy (CWM-FR)", x = "Functional Databases") +
      theme(
        axis.text.x = element_text(size=11, face="bold", color="black", angle=30, hjust=1), 
        axis.text.y = element_text(size=11, face="bold", color="black"), 
        axis.title = element_text(size=13, face="bold"), 
        legend.position="bottom"
      )
    
    write.csv(global_cwm, file.path(out_global_dir, "PlotData_6A_Global_CWM_FR.csv"), row.names = FALSE)
    write.csv(sig_df, file.path(out_global_dir, "PlotData_6A_Global_CWM_FR_Significance.csv"), row.names = FALSE)
    
    ggsave(file.path(out_global_dir, "6A_Plot_Global_CWM_FR_Profile-2.pdf"), plot=p_global_cwm, width=8, height=3.8)
    ggsave(file.path(out_global_dir, "6A_Plot_Global_CWM_FR_Profile-2.png"), plot=p_global_cwm, width=8, height=3.8, dpi=300, bg="white")
  }
  
  
 
  # [图 6B] 全局多维 Beta 解耦面板
  if(length(global_beta_list) > 0) {
    global_beta <- rbindlist(global_beta_list)
    
    # 固定分面面板的出图顺序
    global_beta$Database <- factor(global_beta$Database, levels = db_order)
    
    # 🌟【核心优化 1】：提取全局最大的距离值，用于强制统一 X 和 Y 轴的绝对范围
    max_dist <- max(c(global_beta$Taxa_Dist, global_beta$Func_Dist), na.rm = TRUE)
    
    p_global_beta <- ggplot(global_beta, aes(x = Taxa_Dist, y = Func_Dist)) +
      # 缩小点的大小 (size=1) 和边框 (stroke=0.2)，防止在 8x5 画布下显得像一坨墨迹
      geom_point(aes(fill = Comp_Type), shape=21, size=2.2, alpha=0.7, color="black", stroke=0.2) +
      
      # 细化线条
      geom_smooth(aes(color = Comp_Type), method = "lm", formula = y ~ x, linetype = "solid", linewidth=1, se = FALSE) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#E74C3C", linewidth=1) +
      
      scale_fill_manual(values = c("Intra-group"="#225EA8", "Inter-group"="#F39C12")) + 
      scale_color_manual(values = c("Intra-group"="#225EA8", "Inter-group"="#F39C12")) +
      
      # 🌟【核心优化 2】：X 和 Y 统一使用 [0, max_dist]，且限制 n.breaks = 4 减少数字数量
      scale_x_continuous(limits = c(0, max_dist), expand = expansion(mult = c(0, 0.05)), n.breaks = 4) +
      scale_y_continuous(limits = c(0, max_dist), expand = expansion(mult = c(0, 0.05)), n.breaks = 4) +
      
      # 🌟【核心优化 3】：去掉 scales="free"（默认即为 fixed）。这会隐藏内部重复的坐标轴文本，腾出大量空间！
      facet_wrap(~ Database, ncol=4) + 
      theme_bw() +
      
      # 🌟【核心优化 4】：将符号改回 "R"，避免 rho (ρ) 在图上看起来像 p。同时缩小字体 (size=2.2) 适配小画布
      stat_cor(aes(color = Comp_Type), method="spearman", cor.coef.name = "R", size=3.5, label.x.npc=0.05) +
      
      labs(title = "Global Beta-Diversity Decoupling Landscape", 
           subtitle="Taxonomic vs Functional Dissimilarity (Bray-Curtis)", 
           x = "Taxonomic Distance", 
           y = "Functional Distance") +
      
      theme(
        # 🌟【8x5 画布极限排版】：整体调小字号、边距
        strip.text = element_text(size=11, face="bold", margin=margin(t=3, b=3)), 
        axis.text = element_text(size=11), 
        axis.title = element_text(size=11),
        legend.position = "bottom",
        legend.margin = margin(t=0),      # 缩小图例上边缘
        legend.box.margin = margin(t=-5), # 将图例往上提一点点
        #  aspect.ratio = 1,
        panel.spacing = unit(0.5, "lines") # 统一坐标轴后，面板之间不需要留太多缝隙了
      )
    
    write.csv(global_beta, file.path(out_global_dir, "PlotData_6B_Global_Beta_Decoupling.csv"), row.names = FALSE)
    
    # 严格遵守要求的 8x5 尺寸
    ggsave(file.path(out_global_dir, "6B_Plot_Global_Beta_Decoupling_Facet.pdf"), plot=p_global_beta, width=8, height=4.5)
    ggsave(file.path(out_global_dir, "6B_Plot_Global_Beta_Decoupling_Facet.png"), plot=p_global_beta, width=8, height=4.5, dpi=300, bg="white")
  }
  

  
  

  # 全局系统功能冗余 (Global System FR TDFD) 分面整合图

  
  cat("\n🌐 正在提取各功能模块数据... 渲染全局 TDFD 分面大图 (Global System FR TDFD)...\n")
  
  # 因为 0a 环节没有预设 global_list 数据收集器，所以我们在此动态遍历并重新读取刚才生成的 CSV
  sys_fr_file_list <- list()
  for (func_col in names(target_functions)) {
    func_label <- target_functions[[func_col]]
    f_0a <- file.path(funcunt_Dir, func_label, "PlotData_0a_System_FR_TDFD.csv")
    
    if(file.exists(f_0a)) {
      df_tmp <- read.csv(f_0a)
      df_tmp$Database <- func_label # 为这批数据打上归属标签
      sys_fr_file_list[[func_label]] <- df_tmp
    }
  }
  
  if(length(sys_fr_file_list) > 0) {
    global_sys_fr <- rbindlist(sys_fr_file_list)
    global_sys_fr$Group <- factor(global_sys_fr$Group, levels = target_levels)
    
    # 🌟【修改】：因子重排按照全局指定的数据库顺序
    global_sys_fr$Database <- factor(global_sys_fr$Database, levels = db_order)
    
    p_global_tdfd <- ggplot(global_sys_fr, aes(x = Group, y = System_FR_Index, fill = Group)) +
      geom_boxplot(alpha=0.9, outlier.shape = NA) + 
      geom_jitter(width=0.15, size=1.0, alpha=0.4, color="black") +
      scale_fill_manual(values = group_colors) + 
      
      scale_y_continuous(expand = expansion(mult = c(0.15, 0.3))) + 
      facet_wrap(~ Database, scales = "free_y", ncol = 2) +
      theme_bw() +
      stat_compare_means(method = "t.test", ref.group = "Syn", label = "p.signif", hide.ns = TRUE, vjust = 0.5) +
      labs(
        title = "Global System Functional Redundancy (TD - FD)", 
        y = "System FR Index", 
        x = "Treatment"
      ) +
      theme(
        strip.text = element_text(
          size = 9, 
          face = "bold",
          margin = margin(t = 1, b = 1, unit = "pt")  # t=上边距，b=下边距；数值越大，背景框越高
        ),
        
        # 可选：修饰背景框样式（如圆角、边框颜色等）
        strip.background = element_rect(
          fill = "gray90",    # 背景填充色
          color = "black",   # 边框线条颜色
          linewidth = 0.5    # 边框粗细 (旧版本用 size = 0.5)
        ),  
        axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title = element_text(size = 10, face = "bold"), 
        legend.position = "bottom"
      )
    
    # 固化底层数据并输出高质量图件
    write.csv(global_sys_fr, file.path(out_global_dir, "PlotData_6D_Global_System_FR_TDFD.csv"), row.names = FALSE)
    ggsave(file.path(out_global_dir, "6D_Plot_Global_System_FR_TDFD_Facet.pdf"), plot=p_global_tdfd, width=3.5, height=4.5)
    ggsave(file.path(out_global_dir, "6D_Plot_Global_System_FR_TDFD_Facet.png"), plot=p_global_tdfd, width=3.5, height=4.5, dpi=300, bg="white")
    cat("  -> 成功生成图集: 6D_Plot_Global_System_FR_TDFD_Facet!\n")
  } else {
    cat("  -> 未找到任何 PlotData_0a_System_FR_TDFD.csv 数据，无法生成分面图。\n")
  }
  
  
  
 
  
  
  #### Figure S14 ####
  

  # 加载 ggplot2 和 ggalluvial 绘图包
  if(!require(ggplot2)) install.packages("ggplot2")
  if(!require(ggalluvial)) install.packages("ggalluvial")
  library(ggplot2)
  library(ggalluvial)
  
  
  sankey_data_H18 <- read.csv("/Sankey_Data_H18_vs_Syn.csv", stringsAsFactors = FALSE)
  sankey_data_H20 <- read.csv("/Sankey_Data_H20_vs_Syn.csv", stringsAsFactors = FALSE)
  
  # 定义核心绘图函数
  plot_custom_sankey <- function(df, group_name) {
    # 若数据框为空，跳过绘图
    if(is.null(df) || nrow(df) == 0) {
      cat(sprintf("   - [跳过] %s 组没有数据。\n", group_name))
      return(NULL)
    }
    
    cat(sprintf("   -> 正在生成 %s 桑基图...\n", group_name))
    
    # 1. 数据预处理：填补可能存在的 NA 值
    df <- df %>% mutate(
      Func_Sig = ifelse(is.na(Func_Sig) | Func_Sig == "", "NotSig", Func_Sig),
      Genus_Sig = ifelse(is.na(Genus_Sig) | Genus_Sig == "", "NotSig", Genus_Sig)
    )
    
    # 颜色字典预先放置，供后续提取名称
    genus_colors <- c(
      "Pseudomonas"      = "#FC7BB0",  # 淡紫色 
      "Sphingomonas"     = "#0FA7AD",  # 淡绿色
      "Stenotrophomonas" = "#FCB931",  # 淡橙色
      "Variovorax"       = "#A7D7FF"   # 淡粉色
    )
    

    # 1. 强制设定 Database 的从上到下理想顺序 (确保 antiSMASH 永远在最顶层)
    db_order_ideal <- c("antiSMASH", "KEGG", "CAZy", "NCycDB", "PCycDB", "SCycDB", "FeGenie", "QS")
    
    # 2. 精确计算 Function 的双重聚类顺序 (Database -> 上下调 -> 名称)
    func_order <- df %>%
      select(Database, Function, Func_Sig) %>%
      distinct(Function, .keep_all = TRUE) %>% 
      # 让 Database 按我们上面设计的顺序排
      mutate(Database = factor(Database, levels = db_order_ideal)) %>%
      # 双重排序：先按 Database 聚类，再按 Func_Sig 聚类，最后名称排列
      arrange(Database, factor(Func_Sig, levels = c("Up", "Down", "NotSig")), Function)
    
    func_levels <- as.character(func_order$Function)
    
    # 3. 提取实际存在的 Database 和 Genus，以确保全局不漏
    # 【修改点】：显式调用 base::intersect()，完美解决 conflicted 包的报错冲突
    present_dbs <- base::intersect(db_order_ideal, unique(df$Database))
    present_genus <- base::intersect(names(genus_colors), unique(df$Genus))
    
    # 4. 融合生成全局终极顺序向量 (涵盖三列中所有的唯一节点名称)
    all_stratum_levels <- unique(c(present_dbs, func_levels, present_genus))

    
    # 增加频数列
    df$Freq <- 1
    
    # 将宽格式转换为 ggalluvial 所需的长格式 (lodes)
    df_lodes <- to_lodes_form(df, 
                              key = "x", 
                              value = "stratum", 
                              id = "alluvium", 
                              axes = c("Database", "Function", "Genus"))
    
    # 在长格式状态下，重新对 stratum 赋予我们计算好的全局有序因子！
    df_lodes <- df_lodes %>%
      mutate(stratum = factor(stratum, levels = all_stratum_levels))
    
    # --- 核心逻辑 1：定义每个节点的颜色属性 ---
    df_lodes <- df_lodes %>%
      mutate(
        NodeColor = case_when(
          x == "Database" ~ as.character(stratum),
          x == "Function" ~ as.character(Func_Sig),
          x == "Genus"    ~ as.character(stratum), 
          TRUE ~ "NotSig"
        )
      )
    
    # --- 核心逻辑 2：定义连接线的颜色 ---
    flow_colors <- df_lodes %>%
      filter(x == "Database") %>%
      select(alluvium, FlowColor = stratum)
    
    df_lodes <- df_lodes %>%
      left_join(flow_colors, by = "alluvium")
    
    # --- 颜色映射集 ---
    my_colors <- c(
      banner_colors,         
      genus_colors,          
      "Up"     = "#F78D81",  # 上调
      "Down"   = "#70AFFE",  # 下调
      "NotSig" = "#D9D9D9"   # 不显著
    )
    
    # --- 绘制 ggplot2 桑基图 ---
    p <- ggplot(df_lodes,
                aes(x = x, stratum = stratum, alluvium = alluvium, y = Freq)) +
      
      geom_alluvium(aes(fill = FlowColor), alpha = 0.6, width = 2/5, color = NA) +
      
      geom_stratum(aes(fill = NodeColor), width = 2/5, color = "white", linewidth = 0.5) +
      
      # 调整字体大小使其在 5x5 的紧凑画布中不过于拥挤
      geom_text(stat = "stratum", aes(label = stratum), size = 4.5, color = "black") +
      
      scale_fill_manual(values = my_colors) +
      scale_x_discrete(limits = c("Database", "Function", "Genus"), expand = c(0.15, 0.15)) +
      
      theme_minimal() +
      theme(
        legend.position = "none",          
        panel.grid = element_blank(),      
        axis.text.y = element_blank(),     
        axis.title = element_blank(),      
        axis.text.x = element_text(size = 12, face = "bold", color = "black") 
      ) +
      ggtitle(paste0("Traceability Sankey: ", group_name))
    
    # --- 导出保存 (PDF / PNG) ---
    out_pdf <- paste0(funcunt_Dir, "/Sankey_Plot_", group_name, ".pdf")
    out_png <- paste0(funcunt_Dir, "/Sankey_Plot_", group_name, ".png")
    
    # 【修改点】：彻底还原 5x5 英寸的图纸输出比例
    ggsave(filename = out_pdf, plot = p, width = 12, height = 7.0)
    ggsave(filename = out_png, plot = p, width = 12, height = 7.0, dpi = 300)
    
    cat(sprintf("   => [绘图完成] 图像已保存至:\n      %s\n\n", out_pdf))
    return(p)
  }
  
  # 依次对提取出来的三组数据框进行绘图
  plot_custom_sankey(sankey_data_H18, "H18_vs_Syn")
  plot_custom_sankey(sankey_data_H20, "H20_vs_Syn")

  cat("*** 所有分析及桑基图绘制全部完成！***\n")
  
  
  
  
  #### Figure S15 ####
  
  
  ##### Figure S15A ####
  
  rm(list = ls()) 
  gc() 
  
  # 加载数据处理与可视化核心包
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
  library(ComplexHeatmap) # 用于绘制高阶热图
  library(circlize)       # 配套 ComplexHeatmap 使用的颜色映射包
  library(factoextra)     # 用于提取和可视化 PCA 结果
  library(ggplot2)        # ggsave 依赖此包
  library(ggsci)          # 必须加载 ggsci 才能使用原生 NPG 学术色板函数
  
  # [建议]：长远来看，建议使用 R Project 配合相对路径。当前保留您的绝对路径以确保兼容性。
  setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")
  

  in_DIR_matrix <- "./1_data-analysis/8_分箱径向/4_annotation/11_KEGG-Decoder/1_results/All_42_MAGs_pathway_weight.tsv"
  
  
  
  # 定义主输出目录 (用于存放所有导出的 PDF 和 PNG 矢量图/位图)
  out_DIR_plots <- "./2_analysis_R/3_bin-path/9_KEGG-decoder-95"
  if (!dir.exists(out_DIR_plots)) dir.create(out_DIR_plots, recursive = TRUE)
  
  # 定义子输出目录 (专用于存放处理后的表格数据)
  out_DIR_tables <- file.path(out_DIR_plots, "0_data")
  if (!dir.exists(out_DIR_tables)) dir.create(out_DIR_tables, recursive = TRUE)

  cat("[⚙️] 正在加载代谢矩阵与物种分类数据...\n")
  
  suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
  })
  
  cat("[⚙️] 正在加载代谢矩阵与物种分类数据...\n")
  
  # 1. 读取 KEGG 完整度矩阵 (第一列为原始 MAGs 行名，如 CK-bin.6)
  data <- read.table(in_DIR_matrix, header = TRUE, row.names = 1, sep = "\t", check.names = FALSE)
  
  # 2. 读取更新后的物种分类表
  taxon_file <- "./2_analysis_R/3_bin-path/0_data/3_Taxon_Levels_Completed_95_renamed.tsv"
  taxon_95 <- read.table(taxon_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  

  # 提前记录替换前的原始示例名称，供日志打印对比
  old_example_name <- rownames(data)[1]
  
  # 标准化连字符/下划线，消除 CK-bin.6 与 CK_bin.6 的格式差异
  mat_rownames_clean <- str_replace_all(rownames(data), "-", "_")
  taxon_user_genome_clean <- str_replace_all(taxon_95$user_genome, "-", "_")
  
  # 建立原始矩阵行名与分类表 user_genome 的匹配索引
  matched_idx <- match(mat_rownames_clean, taxon_user_genome_clean)
  
  # 校验是否存在未成功匹配的 MAG
  missing_count <- sum(is.na(matched_idx))
  if (missing_count > 0) {
    warning(sprintf("[⚠️] 警告：有 %d 个 MAG 在分类表中未找到匹配，将保留原始名称！", missing_count))
  }
  
  # 提取对应的 new_bin_name 并更新 data 的行名
  new_names <- taxon_95$new_bin_name[matched_idx]
  # 仅对成功匹配到的行名进行替换，未匹配到的保留原名
  rownames(data)[!is.na(new_names)] <- new_names[!is.na(new_names)]
  
  # 生成同步更新行名后的数值矩阵 (mat)
  mat <- as.matrix(data)
  
  
  assign_category <- function(pathways) {
    cat <- rep("Other Metabolism", length(pathways))
    

    # 含有 dehydrogenase 的通路（如甲醇脱氢酶等）先归为此类，后续会被更特异的循环精准覆盖
    cat[grepl("hydrogenase|hydrogen:", pathways, ignore.case=TRUE)] <- "Hydrogen Metabolism"
    
    # 基础氨基酸匹配（后续特异系统如 Twin Arginine 会将其正确覆盖）
    cat[grepl("histidine|arginine|lysine|serine|threonine|asparagine|glutamine|cysteine|glycine|proline|alanine|valine|methionine|phenylalanine|isoleucine|leucine|tryptophan|tyrosine|aspartate|glutamate", pathways, ignore.case=TRUE)] <- "Amino Acid Metabolism"

    # 1. 基础能量与物质转运
    cat[grepl("Cytochrome|ATPase|oxidoreductase|Photosystem|reaction center|b6/f complex", pathways, ignore.case=TRUE)] <- "Energy & Electron Transport"
    cat[grepl("transporter|abc-type", pathways, ignore.case=TRUE)] <- "Transporters"
    
    # 2. 分泌系统与植物定殖（实现解耦，确保 TAT 与 I-VI 型分泌系统独立归类）
    cat[grepl("Sec-SRP|Secretion|Twin Arginine", pathways, ignore.case=TRUE)] <- "Secretion Systems"
    cat[grepl("Flagellum|Chemotaxis|Adhesion|Biofilm|Curli|Colanic|Competence", pathways, ignore.case=TRUE)] <- "Plant Colonization & Interaction"
    
    # 3. 维生素与多糖降解（优化了果胶酶 galactur* 的匹配，防止漏网）
    cat[grepl("thiamin|riboflavin|cobalamin", pathways, ignore.case=TRUE)] <- "Vitamins Biosynthesis"
    cat[grepl("cellulase|chitinase|amylase|glucosidase|pullulanase|pectinesterase|deacetylase|glucoamylase|galactur|hexosaminidase|oligogalacturonide lyase", pathways, ignore.case=TRUE)] <- "Carbohydrate Degradation"
    
    # 4. 碳代谢与抗逆储备（精准匹配 \\bCBB\\b 杜绝误伤 cbb3 氧化酶；收纳了 3-HP、4-HB 固碳途径及补缺基因）
    cat[grepl("glycolysis|gluconeogenesis|TCA|RuBisCo|\\bCBB\\b|CBB Cycle|rTCA|Wood-Ljungdahl|Glyoxylate|Mixed acid|Entner-Doudoroff|starch/glycogen|Anaplerotic|Hydroxypropionate|Hydroxybutyrate", pathways, ignore.case=TRUE)] <- "Carbon Metabolism"
    cat[grepl("polyhydroxybutyrate|polyphosphate|carotenoid|astaxanthin|nostoxanthin|zeaxanthin|myxoxanthophylls|staphyloaxanthin|mevalonate|MEP-DOXP|Retinal|Arsenic|Xenobiotics|degradation to|PET degradation|Sulfolipid", pathways, ignore.case=TRUE)] <- "Stress Tolerance & Storage"
    
    # 5. 主循环元素代谢（置于最底部，拥有绝对覆盖权，彻底回收氮/硫/一碳循环中的特异脱氢酶）
    cat[grepl("C-P lyase|CP-lyase|phosphonate|phosphate", pathways, ignore.case=TRUE)] <- "Phosphorus Metabolism"
    # 引入 nitric/nitrous 修正反硝化步骤漏网；hydrazine 重新覆盖氢代谢
    cat[grepl("nitrite|nitrate|ammonia|nitrogen|hydroxylamine|hydrazine|DNRA|nitric|nitrous", pathways, ignore.case=TRUE)] <- "Nitrogen Cycle"
    # 硫特异性酶从氢代谢中收回
    cat[grepl("sulfate|sulfite|sulfide|thiosulfate|sulfur|DMSP|DMS|DMSO", pathways, ignore.case=TRUE)] <- "Sulfur Cycle"
    # 引入 Coenzyme、formaldehyde 和 alcohol oxidase，完美收拢一碳与产甲烷核心酶
    cat[grepl("Methanogenesis|methane|methanol|methylamine|dimethylamine|trimethylamine|formaldehyde|Coenzyme|alcohol oxidase", pathways, ignore.case=TRUE)] <- "Methane Metabolism"
    
    return(cat)
  }
  
  # 获取所有列 (通路) 对应的全新宏观类别标签
  col_categories <- assign_category(colnames(mat))
  
  
  
  
  cat("正在生成宏观大类汇总热图...\n")
  # 将宽表拉长，按分类聚合计算均值，再重构为精简矩阵
  mat_summary <- as.data.frame(mat) %>% 
    rownames_to_column("MAG") %>% 
    pivot_longer(cols = -MAG, names_to = "Pathway", values_to = "Completeness") %>%
    mutate(Category = assign_category(Pathway)) %>%
    group_by(MAG, Category) %>%
    summarize(Mean_Completeness = mean(Completeness, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = Category, values_from = Mean_Completeness) %>%
    column_to_rownames("MAG") %>%
    as.matrix()
  
  
  
  # 1. 甄选核心核心生源要素与养分获取轴（避免非代谢通路污染生态位定义）
  niche_dimensions <- c("Carbon Metabolism", "Carbohydrate Degradation", 
                        "Nitrogen Cycle", "Sulfur Cycle", "Phosphorus Metabolism", 
                        "Amino Acid Metabolism", "Methane Metabolism")
  
  # 2. 从多维大类均值矩阵中提炼出无偏误的百分比宽度得分
  genomic_niche_df <- as.data.frame(mat_summary) %>%
    rownames_to_column("MAG_ID") %>%
    # 仅抽取核心养分利用大类
    dplyr::select(MAG_ID, all_of(intersect(niche_dimensions, colnames(mat_summary)))) %>%
    # 将原始矩阵中 0-1 范围的完整度均值转换为标准百分比
    dplyr::mutate(across(-MAG_ID, ~ .x * 100))
  
  # 3. 彻底拦截统计陷阱：使用各大类百分比的 mean() 构建超级基因组生态位宽度指数
  super_genomic_breadth <- genomic_niche_df %>%
    rowwise() %>%
    mutate(
      # 平衡赋权：每个生源要素轴对总得分贡献完全对等，彻底消除通路数量多寡的偏倚
      Super_Genomic_Breadth = mean(c_across(-MAG_ID), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # 按照超级基因组生态位广度值进行降序排序
    arrange(desc(Super_Genomic_Breadth)) %>%
    # 💡 字符替换对齐：将 MAG_ID 里的旧标签统一更新为您的学术论文指定标签 (H18->S.Pse, H20->S.Sph)
    mutate(MAG_ID = gsub("H18", "S.Pse.", MAG_ID),
           MAG_ID = gsub("H20", "S.Sph.", MAG_ID)) %>%
    # 使用 rev() 锁定因子层级，确保在下文 coord_flip() 翻转后，全域泛化种（通才）位居图表最上方
    mutate(MAG_ID = factor(MAG_ID, levels = rev(unique(MAG_ID))))
  
  # 4. 导出完全清洗、可用于直接做学术表格的功能生态位得分文本
  output_niche_table <- file.path(out_DIR_tables, "All_42_MAGs_Genomic_Niche_Breadth_Scores.tsv")
  write.table(super_genomic_breadth, file = output_niche_table, sep = "\t", quote = FALSE, row.names = FALSE)
  cat(paste0("[💾] 基因组生态位量化指标已成功下线至: ", output_niche_table, "\n"))
  
  # 5. 绘制全域横向长图（利用 ggplot2 机制）
  p_genomic_super <- ggplot(super_genomic_breadth, aes(x = MAG_ID, y = Super_Genomic_Breadth)) +
    geom_bar(stat = "identity", fill = "#FDA403", color = "black", linewidth = 0.3) +
    labs(title = "Super Genomic Niche Breadth (KEGG-Decoder Profiles Combined)",
         subtitle = "Identification of Genomic Generalists (r-strategists) vs Specialists (K-strategists)",
         x = "MAG ID", 
         y = "Super Genomic Niche Breadth Index (Mean % across Core Resource Axes)") + 
    
    coord_flip() + # 坐标轴无损翻转，纵向高瘦型条形图完美解放 MAG ID 排布
    
    theme_minimal() + # 维持您的画风一致性
    theme(
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 13, color = "black"),
      axis.title.x = element_text(size = 11, face = "bold", color = "black"),
      axis.title.y = element_text(size = 11, face = "bold", color = "black"),
      axis.text.y = element_text(size = 11, color = "grey20"), 
      axis.text.x = element_text(size = 10, color = "grey20", face = "bold")
    )
  
  # 6. 精细化尺寸无损导出（纵向 42 个 MAG 的长图推荐高度设为 8，宽度设为 6，呼吸感十足）
  ggsave(file.path(out_DIR_plots, "4_Super_Genomic_Niche_Breadth_Ranking.pdf"), plot = p_genomic_super, width = 4.5, height = 8)
  ggsave(file.path(out_DIR_plots, "4_Super_Genomic_Niche_Breadth_Ranking.png"), plot = p_genomic_super, width = 4.5, height = 8, dpi = 300)
  
  cat("[SUCCESS] 全剧终！新增代谢通道生态位广度图表已高无损导出，模块顺利通关！\n")
  
  
  
  

  
  
##### Figure S15B ####
rm(list = ls()); gc()

# 加载必要包
pacman::p_load(data.table, tidyverse, pheatmap, ggrepel, ggsci, 
               FactoMineR, factoextra, viridis, patchwork)

# 工作目录设置
setwd("D:/work_year/5_宏基因组-RNAseq/4_metadata_analysis/")
data_DIR <- "./1_data-analysis/8_分箱径向/5_gapseq-COBRApy-99/"
out_DIR  <- "./2_analysis_R/3_bin-path/12_gapseqCOBRApyMICOM/"
if (!dir.exists(out_DIR)) dir.create(out_DIR, recursive = TRUE)

# 定义全局美学主题 (统一控制字体、颜色、字号，方便后期微调)
my_theme <- theme_bw() +
  theme(
    axis.text = element_text(size = 11, color = "black"),           # 坐标轴刻度字体
    axis.title = element_text(size = 11, face = "bold", color = "black"), # 坐标轴标题字体
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),     # 主标题字体
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30"),
    legend.text = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank() # 去除次级网格线，画面更干净
  )


EXC_fluxes  <- fread(paste0(data_DIR, "1_gapseq_models/RhizoDroughtAllcondition_all_mags_exchange_fluxes.csv"), quote = "")
Fba_summary <- fread(paste0(data_DIR, "1_gapseq_models/RhizoDroughtAllcondition_all_mags_fba_summary.csv"), quote = "", na.strings = c("NA", "N/A", ""))

dir_fba <- paste0(out_DIR, "1_FBA_Summary/")
if (!dir.exists(dir_fba)) dir.create(dir_fba)

# 衍生科学指标：数据清洗与计算
fba_clean0 <- Fba_summary %>%
  # 1. 替换 Stress_Growth_Rate 中的 NA 为 0
  replace_na(list(Stress_Growth_Rate = 0)) %>%
  # 2. 过滤掉完全不生长的死菌 (避免分母为0)
  filter(Base_Growth_Rate > 0.001) %>%
  # 3. 计算抗逆韧性
  mutate(Resilience_Ratio = Stress_Growth_Rate / Base_Growth_Rate)


threshold_base <- 0.1
threshold_stress <- 0.05

# 读取更新后的重命名映射表
rename <- read.table( "./2_analysis_R/3_bin-path/0_data/3_Taxon_Levels_Completed_99_renamed.tsv", 
                     sep = "\t", header = TRUE, stringsAsFactors = FALSE)

mag_id_clean <- fba_clean0$MAG_ID %>% 
  str_replace_all("S\\.Pse\\.", "H18") %>% 
  str_replace_all("S\\.Sph\\.", "H20") %>% 
  str_replace_all("[\\.\\-]", "_")


user_genome_clean <- rename$user_genome %>% 
  str_replace_all("[\\.\\-]", "_")

matched_idx <- match(mag_id_clean, user_genome_clean)

fba_clean <- fba_clean0 %>% 
  mutate(

    MAG_ID = ifelse(!is.na(matched_idx), rename$new_bin_name[matched_idx], MAG_ID)
  )

# 5. 匹配质量检查与提示
missing_count <- sum(is.na(matched_idx))
if (missing_count > 0) {
  warning(sprintf("[⚠️] 警告：有 %d 个 MAG 在分类重命名表中未找到匹配，已保留原名称！", missing_count))
} else {
  cat("[✨] 所有 fba_clean 中的 MAGs 名称已成功替换为全新的 new_bin_name！\n")
}
  
  
my_theme <- theme_bw() +
  theme(
    axis.text = element_text(size = 11, color = "black"),           # 坐标轴刻度字体
    axis.title = element_text(size = 11, face = "bold", color = "black"), # 坐标轴标题字体
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),     # 主标题字体
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey30"),
    legend.text = element_text(size = 11, color = "black"),
    legend.title = element_text(size = 11, face = "bold"),
    panel.grid.minor = element_blank() # 去除次级网格线，画面更干净
  )



p1_scatter <- ggplot(fba_clean, aes(x = Base_Growth_Rate, y = Stress_Growth_Rate)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey60", linewidth = 0.8) + 
  # 绘制散点
  geom_point(aes(fill = Resilience_Ratio, size = Genes_Count), shape = 21, color = "black", alpha = 0.8) +
  # 添加右上角优势菌的文本标签
  geom_text_repel(data = subset(fba_clean, Base_Growth_Rate > threshold_base & Stress_Growth_Rate > threshold_stress),
                  aes(label = MAG_ID), 
                  size = 3.0, fontface = "bold", color = "darkred", 
                  box.padding = 0.5, max.overlaps = 10) +
  scale_fill_viridis(option = "magma", name = "Stress/Base\nRatio") +
  
  # 【核心新增代码】：限制点大小的映射范围（例如最小为 2，最大为 8）
  scale_size_continuous(range = c(0.25, 5), name = "Gene Count") +
  
  labs(
    title = "Growth Resilience of Microbiome MAGs", 
    subtitle = paste0("Highlighted: Base > ", threshold_base, " & Stress > ", threshold_stress),
    # 使用 expression 生成带上标的 h^-1
    x = expression("Base Growth Rate (" * h^-1 * ")"), 
    y = expression("Stress Growth Rate (" * h^-1 * ")")
  ) +
   my_theme 
# +
#   # 强制正方形画布
#   coord_fixed(ratio = 1) 

# 保存图片
ggsave(paste0(dir_fba, "1A_Resilience_Scatter.pdf"), p1_scatter, width = 4.5, height = 3)
ggsave(paste0(dir_fba, "1A_Resilience_Scatter.png"), p1_scatter, width = 4.5, height = 3, dpi = 300)







#### Figure S16 ####


"Same as Figure 5"








