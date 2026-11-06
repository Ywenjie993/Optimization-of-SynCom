#### Scripts for SynCom Optimization Paper ####


####******************************************************####
#### Figure 1 ####
#### Figure 1A #####
rm(list = ls())
gc()
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/4_WAZ")

##导入数据
df <- read.csv('./4_historigram_D/D值.csv',row.names = 1)
df1 <- df[, -1, drop = F]



# 加载必要的包
library(ggplot2)

# 1. 数据准备和检查
# 假设数据框名为df，数值列名为value（请根据实际情况修改）
# 如果不知道列名，可以先查看数据框结构
print("数据框结构:")
str(df1)

print("数据摘要:")
summary(df1)

# 查看列名（如果不知道数值列的名称）
print("所有列名:")
colnames(df1)

# 假设数值列名为"value"，如果不是，请修改下面的代码
# 例如：如果列名为"score"，则将下面所有的df$value改为df$score
value_column <- "D.value"  # 请修改为你的实际列名

# 检查数据基本情况
cat("\n=== 数据统计信息 ===\n")
cat("数据量:", length(df1[[value_column]]), "\n")
cat("缺失值数量:", sum(is.na(df1[[value_column]])), "\n")
cat("数据范围:", range(df1[[value_column]], na.rm = TRUE), "\n")
cat("均值:", mean(df1[[value_column]], na.rm = TRUE), "\n")
cat("中位数:", median(df1[[value_column]], na.rm = TRUE), "\n")
cat("标准差:", sd(df1[[value_column]], na.rm = TRUE), "\n")




stats_hist <- ggplot(df1, aes(x = .data[[value_column]])) +
  
  # 绘制直方图
  geom_histogram(
    bins = 25,                    # 设置直方图的箱数（分组数量）
    fill = "#91CEEC",            # 设置直方图的填充颜色（绿色）
    color = "black",             # 设置直方图边框颜色
    alpha = 1,                 # 设置填充透明度（0-1，1为完全不透明）
    linewidth = 0.1              # 设置边框线粗细
  ) +
  
  
  # 添加统计信息文本注释
  annotate("text",
           x = 0.5, y = Inf,     # 设置文本位置（x=0.05靠近左边缘，y=Inf表示顶部）
           label = paste(          # 拼接统计信息字符串
             "Statistics:\n",      # 标题
             "n = ", length(na.omit(df1[[value_column]])), "\n",      # 样本数量
             "Mean = ", round(mean(df1[[value_column]], na.rm = TRUE), 3), "\n",  # 均值（保留3位小数）
             "Median = ", round(median(df1[[value_column]], na.rm = TRUE), 3), "\n",  # 中位数
             "SD = ", round(sd(df1[[value_column]], na.rm = TRUE), 3), "\n",      # 标准差
             "Min = ", round(min(df1[[value_column]], na.rm = TRUE), 3), "\n",    # 最小值
             "Max = ", round(max(df1[[value_column]], na.rm = TRUE), 3)           # 最大值
           ),
           hjust = 0,              # 水平对齐方式：0=左对齐，1=右对齐，0.5=居中
           vjust = 1.2,            # 垂直对齐方式：1=底部对齐，0=顶部对齐
           size = 3.5,             # 字体大小
           color = "black"         # 字体颜色
  ) +
  
  # 设置坐标轴标签和标题
  labs(
    x = "Overall Drought Resistance",  # x轴标签：综合抗旱能力
    y = "Frequency",                   # y轴标签：频数
    title = ""  # 图形标题
  ) +
  
  # 使用经典主题（只有坐标轴，没有面板边框）
  theme_classic() +
  
  # 自定义主题细节
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # 标题居中、14号、粗体
    axis.title = element_text(size = 12),                              # 坐标轴标题12号
    axis.text = element_text(size = 10),                               # 坐标轴刻度标签10号
    
    # 确保坐标轴线显示
    axis.line = element_line(color = "black", linewidth = 0.5),        # 坐标轴线黑色，粗细0.5
    
    # 确保刻度线显示
    axis.ticks = element_line(color = "black", linewidth = 0.5),       # 刻度线黑色，粗细0.5
    
    # 移除所有网格线
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.major.y = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor.x = element_line(color = "gray95", linewidth = 0.1),
    panel.grid.minor.y = element_line(color = "gray95", linewidth = 0.1),                                      # 清除所有网格线
    
    # 移除面板边框（theme_classic已处理，这里确保一下）
    panel.border = element_blank()                                     # 清除面板边框
  ) +
  
  # 设置坐标轴范围：使用coord_cartesian只改变显示范围，不删除数据
  coord_cartesian(xlim = c(0, 0.8)) +                                    # x轴显示范围为0-1
  
  # 设置x轴刻度
  scale_x_continuous(
    breaks = seq(0, 0.8, by = 0.2)                                       # x轴刻度：0, 0.2, 0.4, 0.6, 0.8, 1.0
  )  +
  # 设置y轴刻度
  scale_y_continuous(
    breaks = seq(0, 25, by = 5),
    expand = expansion(mult = c(0, 0))  # 上下都不扩展
  )

# 显示图形
print(stats_hist)



# 5. 保存图形
# 创建保存目录（如果不存在）
output_dir <- "./4_historigram_D"



# 保存统计信息直方图
ggsave(
  filename = file.path(output_dir, "统计信息直方图-2.png"),
  plot = stats_hist,
  width = 3,
  height = 3,
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(output_dir, "统计信息直方图.pdf"),  # 文件名
  plot = stats_hist,                              # 要保存的图形对象
  width = 3.0,                                      # 宽度（英寸）
  height = 3,                                     # 高度（英寸）
  device = "pdf",                                 # 输出格式
  dpi = 300                                       # 分辨率
)



#### Figure 1B #####

rm(list = ls())
gc()
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/4_WAZ")





# 加载必要的R包
library(ggplot2)   # 数据可视化
library(tidyr)     # 数据整理
library(dplyr)     # 数据处理
library(scales)    # 坐标轴格式化
library(grDevices) # 图形设备控制

# 1. 数据准备部分

# 创建示例数据集
# 包含三个样本的6个生理指标（PH, RL, AFW, BFW, ADW, BDW）和D_value
data <- read.table('./4_historigram_D/1B.txt', header = T, sep = "\t")

# 2. 数据预处理

# 【可选优化】：如果你希望 X 轴的样本顺序也保持原样（不按字母顺序），加上这一行
data$Material <- factor(data$Material, levels = unique(data$Material))

# 将宽格式数据转换为长格式，便于ggplot2绘制分组柱形图
data_long <- data %>%
  pivot_longer(
    cols = c(PH, RL, SFW, RFW, SDW, RDW),  # 需要转换的列
    names_to = "Variable",                 # 新列名：变量类型
    values_to = "Value"                    # 新列名：变量数值
  ) %>%
  # 【核心修改点】：将 Variable 转换为因子，按原始特征列表固定顺序
  mutate(Variable = factor(Variable, levels = c("PH", "RL", "SFW", "SDW", "RFW", "RDW")))

# 3. 颜色映射定义

# 为每个生理指标定义特定的颜色
# 颜色选择基于视觉区分度和美观性
color_mapping <- c(
  'PH'  = '#E3AE99',   # 粉色，表示PH指标
  'RL'  = '#EDC894',   # 粉红色，表示RL指标
  'SFW' = '#ACD7C8',   # 蓝绿色，表示AFW指标
  'RFW' = '#92CEEC',   # 天蓝色，表示BFW指标
  'SDW' = '#65C2A9',   # 浅蓝色，表示ADW指标
  'RDW' = '#8AB9E3'    # 深蓝色，表示BDW指标
)

# 4. 创建图形对象

# 使用ggplot2创建基础绘图对象
# 注意：ggplot()创建空画布，后续通过+添加图层
p <- ggplot() +
  
  # 4.1 添加分组柱形图层（生理指标数据）

# 绘制三个样本的6个生理指标柱形图
geom_bar(
  data = data_long,                    # 数据源：长格式数据
  aes(x = Material,                    # X轴：样本名称
      y = Value,                       # Y轴：生理指标数值
      fill = Variable),                # 填充颜色：区分不同指标
  stat = "identity",                   # 使用原始数据值，不进行统计汇总
  position = position_dodge(width = 0.8), # 分组柱形：柱子并排显示，间距0.8
  width = 0.7,                         # 每个柱子的宽度
  alpha = 0.8                          # 透明度：80%（使颜色更柔和）
) +
  
  # 4.2 添加D_value趋势线图层（使用第二个Y轴）

# 绘制连接三个样本D_value的折线
geom_line(
  data = data,                         # 数据源：原始宽格式数据
  aes(x = Material,                    # X轴：样本名称（与柱形图对齐）
      y = D_value * 4,                 # Y轴：D_value缩放4倍以匹配主Y轴范围
      group = 1),                      # 将所有点连接成一条连续的线
  color = "#FF6B6B",                   # 线条颜色：醒目的红色
  size = 1.5,                          # 线条粗细：1.5
  lineend = "round"                    # 线条端点样式：圆形（更美观）
) +
  
  # 4.3 添加趋势线上的点标记

# 在折线的每个数据点位置添加标记点
geom_point(
  data = data,                         # 数据源：原始数据
  aes(x = Material,                    # X轴位置：样本名称
      y = D_value * 4),                # Y轴位置：缩放后的D_value
  color = "#FF6B6B",                   # 点颜色：与折线颜色一致
  size = 4,                            # 点的大小
  shape = 18                           # 点的形状：菱形（18号形状）
) +
  
  # 4.4 添加D_value数据标签

# 在每个数据点上方显示具体的D_value数值
geom_text(
  data = data,                         # 数据源：原始数据
  aes(x = Material,                    # X轴位置：样本名称
      y = D_value * 4,                 # Y轴位置：缩放后的D_value
      label = sprintf("%.3f", D_value)), # 标签内容：保留3位小数
  vjust = -1,                          # 垂直位置：向上移动1个单位
  color = "#FF6B6B",                   # 标签颜色：红色
  size = 4,                            # 字体大小：4
  fontface = "bold"                    # 字体样式：粗体（更醒目）
) +
  
  # 5. 图形美学设置
  
# 5.1 设置柱形图颜色映射
scale_fill_manual(values = color_mapping) +
  # 5.2 设置双Y轴系统
  scale_y_continuous(
    name = "DRI",                        # 左侧Y轴名称：生理指标值
    
    # 设置Y轴范围从0开始
    limits = c(0, NA),                   # 下限为0，上限自动计算
    
    # 控制扩展：底部不扩展，顶部扩展5%
    expand = expansion(mult = c(0, 0.05)),
    
    # 关键：设置左侧Y轴标签格式，保留2位小数
    labels = function(x) sprintf("%.2f", x),  # 保留2位小数
    
    # 创建第二个Y轴（右侧）- 只能有一个sec.axis参数
    sec.axis = sec_axis(
      ~./4,                              # 转换函数：将显示值除以4
      name = "D_value",                  # 右侧Y轴名称
      
      # 可选：设置右侧Y轴标签格式
      labels = function(x) sprintf("%.2f", x)  # 右侧Y轴保留2位小数
    )
  ) +
  
  # 5.3 设置图形标签
  labs(
    title = "",                          # 主标题（留空）
    x = "",                              # X轴标签（留空）
    fill = "Physiological Traits"        # 图例标题：生理指标
  ) +
  
  # 5.4 使用guides()函数精确控制图例

# guide_legend：创建自定义图例指南
guides(
  fill = guide_legend(
    # 图例标题设置
    title = "Physiological Traits",    # 图例标题
    title.position = "top",            # 标题位置：图例上方
    title.hjust = 0.5,                 # 标题水平居中
    
    # 图例排列设置
    nrow = 1,                          # 强制单行排列
    direction = "horizontal",          # 水平方向排列
    
    # 图例项目布局
    label.position = "bottom",         # 标签位置：图例符号下方
    label.hjust = 0.5,                 # 标签水平居中
    
    # 图例符号尺寸
    keywidth = unit(1.0, "cm"),        # 图例符号宽度
    keyheight = unit(0.5, "cm"),       # 图例符号高度
    
    # 图例符号外观（覆盖默认设置）
    override.aes = list(
      alpha = 0.8,                     # 透明度与柱形图一致
      size = 0.5                       # 符号大小
    )
  )
) +
  
  # 6. 图形主题和样式设置
  
theme_minimal(base_size = 10) +        # 使用minimal主题，基础字体大小12
  theme(
    # 6.1 标题样式
    plot.title = element_text(
      hjust = 0.5,                       # 水平居中
      size = 12,                         # 字体大小16
      face = "bold"                      # 粗体
    ),
    
    # 6.2 Y轴样式设置 - 修改字体大小
    axis.title.y.left = element_text(
      color = "#2196F3",                 # 左侧Y轴标题颜色：蓝色
      size = 11,                         # 修改：字体大小14
      face = "bold",                     # 粗体
      margin = margin(r = 10)            # 可选：右侧边距10像素
    ),
    axis.text.y.left = element_text(
      color = "#2196F3",                 # 左侧Y轴刻度颜色：蓝色
      size = 10                          # 修改：刻度标签大小12
    ),
    axis.title.y.right = element_text(
      color = "#FF6B6B",                 # 右侧Y轴标题颜色：红色
      size = 11,                         # 修改：字体大小14
      face = "bold",                     # 粗体
      margin = margin(l = 10)            # 可选：左侧边距10像素
    ),
    axis.text.y.right = element_text(
      color = "#FF6B6B",                 # 右侧Y轴刻度颜色：红色
      size = 10                          # 修改：刻度标签大小12
    ),
    
    # 6.3 X轴样式设置 - 修改字体大小及旋转
    axis.text.x = element_text(
      size = 10,                         # 字体大小
      face = "bold",                     # 粗体
      angle = 20,                        # 【新增】将标签旋转30度
      hjust = 1,                         # 【新增】水平对齐，确保文字贴合刻度线
      vjust = 1,                         # 【新增】垂直对齐，微调文字位置
      margin = margin(t = 5)             # 顶部边距5像素
    ),
    
    # 6.4 网格线设置
    panel.grid.major = element_line(
      color = "grey90",                  # 主要网格线颜色：浅灰色
      linewidth = 0.3                    # 线宽：0.3
    ),
    panel.grid.minor = element_blank(),  # 移除次要网格线
    
    # 6.5 图例整体设置
    legend.position = "bottom",          # 图例位置：底部
    legend.box.margin = margin(t = 0, b = 5), # 图例上下边距
    legend.box.spacing = unit(0.2, "cm"),# 图例与绘图区的间距
    
    # 6.6 背景设置
    plot.background = element_rect(
      fill = "white",                    # 绘图区域背景：白色
      color = NA                         # 无边框
    ),
    panel.background = element_rect(
      fill = "white",                    # 面板背景：白色
      color = NA                         # 无边框
    ),
    
    # 6.7 添加坐标轴轴线
    axis.line = element_line(
      color = "black",                   # 轴线颜色：黑色
      size = 0.5,                        # 轴线粗细：0.5
      lineend = "butt"                   # 轴线端点：平头
    )
  )

# 7. 显示图形（在RStudio中预览）

print(p)

# 8. 导出图形到文件


# 8.1 导出为PDF格式（矢量图，适合印刷和出版）

# 定义PDF文件参数
pdf_file <- "./5_总分同比展示图/Physiological_Traits_Analysis.pdf"

# 开启PDF图形设备
pdf(
  file = pdf_file,       # 输出文件名
  width = 3.5,            # 宽度：10英寸
  height = 4,            # 高度：7英寸
  pointsize = 12         # 基础字体大小
)

# 绘制图形到PDF设备
print(p)

# 关闭PDF设备（重要：确保文件正确保存）
dev.off()

# 输出提示信息
cat("✓ PDF文件已保存：", pdf_file, "\n")

# 8.2 导出为PNG格式（位图，适合网页和演示）

# 定义PNG文件参数
png_file <- "./5_总分同比展示图/Physiological_Traits_Analysis.png"

# 开启PNG图形设备
png(
  filename = png_file,   # 输出文件名
  width = 2400,          # 宽度：2400像素（高分辨率）
  height = 1680,         # 高度：1680像素
  res = 300,             # 分辨率：300 DPI（印刷质量）
  pointsize = 12         # 基础字体大小
)

# 绘制图形到PNG设备
print(p)

# 关闭PNG设备
dev.off()

# 输出提示信息
cat("✓ PNG文件已保存：", png_file, "\n")



#### Figure 1D ####

# 清理R环境：移除所有对象+释放内存（确保脚本可重复运行）
rm(list = ls())
gc()

# 加载必要的包
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# 设置工作目录（需根据实际路径修改）
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/4_WAZ")

# 读取数据（制表符分隔，含表头）
data_Drought_0 <- read.table("./8_双组直方图/DRI_drought.txt", header = T, sep = "\t")

data_Syn <- read.table("./8_双组直方图/DRI_Syn.txt", header = T, sep = "\t")
data_Drought <- data_Drought_0[data_Drought_0$Material %in% data_Syn$Material,]





# 准备数据 - 将两个数据框合并为长格式
# 目的：将Syn和Drought的数据按变量对应合并，便于后续绘图
data_combined <- data.frame(
  Material = data_Syn$Material,  # 材料ID列
  
  # PH 对比
  PH_Syn = data_Syn$PH_Syn,      # Syn组的PH数据
  PH_Drought = data_Drought$PH,  # Drought组的PH数据
  
  # RL 对比
  RL_Syn = data_Syn$RL_Syn,      # Syn组的RL数据
  RL_Drought = data_Drought$RL,  # Drought组的RL数据
  
  # AFW 对比
  AFW_Syn = data_Syn$AFW_Syn,    # Syn组的AFW数据
  AFW_Drought = data_Drought$AFW, # Drought组的AFW数据
  
  # BFW 对比
  BFW_Syn = data_Syn$BFW_Syn,    # Syn组的BFW数据
  BFW_Drought = data_Drought$BFW, # Drought组的BFW数据
  
  # ADW 对比
  ADW_Syn = data_Syn$ADW_Syn,    # Syn组的ADW数据
  ADW_Drought = data_Drought$ADW, # Drought组的ADW数据
  
  # BDW 对比
  BDW_Syn = data_Syn$BDW_Syn,    # Syn组的BDW数据
  BDW_Drought = data_Drought$BDW  # Drought组的BDW数据
)

# 定义重叠直方图绘制函数
# 参数说明:
#   syn_data: Syn组数据向量
#   drought_data: Drought组数据向量  
#   syn_name: Syn组变量名（用于参考，函数内部未直接使用）
#   drought_name: Drought组变量名（用于参考，函数内部未直接使用）
#   title: 图形标题


plot_overlap_hist <- function(syn_data, drought_data, syn_name, drought_name, title) {
  
  # 1. 数据准备: 将两组数据合并为一个长格式数据框
  plot_data <- data.frame(
    value = c(syn_data, drought_data),  # 合并所有数据值
    group = rep(c("Syn", "Drought"), each = length(syn_data))  # 创建分组标签
  )
  
  # 2. 将分组因子化，确保正确的顺序
  plot_data$group <- factor(plot_data$group, levels = c("Syn", "Drought"))
  
  # 3. 创建ggplot对象
  ggplot(plot_data, aes(x = value, fill = group)) +
    
    # 4. 添加直方图层（频率版本）
    geom_histogram(
      alpha = 0.6,                   # 增加透明度确保重叠区域可见
      position = "identity",         # 直方图位置设置为重叠（非堆叠）
      bins = 30,                     # 设置直方图柱子数量为30
      color = "black",               # 柱子边框颜色为黑色
      linewidth = 0.1                # 柱子边框线宽
    ) +
    
    # 5. 设置填充颜色映射（确保图例和实际颜色一致）
    scale_fill_manual(
      name = "Treatment",            # 图例标题
      #  values = c("Syn" = "#E44A33", "Drought" = "#4DBAD6"),  # 指定颜色
      values = c("Syn" = "#EBA48F", "Drought" = "#75AAD2"),  # 您指定的颜色
      
      labels = c("Syn", "Drought"),  # 图例标签
      breaks = c("Syn", "Drought")   # 确保图例顺序与颜色映射一致
    ) +
    
    # 6. 设置图形标签和标题（这里设置了要显示的内容）
    labs(
      title = title,                 # 主标题（显示）
      x = "Value",                   # x轴标签（显示）
      y = "Frequency",               # y轴标签（显示频率）
      fill = ""                      # 图例标题：空（使用scale_fill_manual中的name）
    ) +
    
    # 7. 设置Y轴刻度从0开始
    scale_y_continuous(
      expand = expansion(mult = c(0, 0.1)),  # Y轴从0开始，顶部留10%的扩展空间
      limits = c(0, NA)                      # 明确设置下限为0，上限自动确定
    ) +
    
    # 8. 设置图形主题
    theme_minimal() +                # 使用简洁的minimal主题
    
    # 9. 自定义主题细节
    theme(
      # 标题设置 - 修改：显示标题
      plot.title = element_text(     # 原为element_blank()
        hjust = 0.5,                 # 标题水平居中
        size = 12,                   # 标题字体大小
        face = "bold",               # 标题字体加粗
        margin = margin(b = 10)      # 标题底部边距
      ),
      
      # 图例设置
      legend.position = "top",       # 图例位置：顶部
      legend.title = element_text(
        size = 10,                   # 图例标题字体大小
        face = "bold",               # 图例标题加粗
        margin = margin(b = 2)       # 图例标题底部边距
      ),
      legend.text = element_text(
        size = 9,                    # 图例标签字体大小
        margin = margin(r = 10)      # 图例标签右侧边距
      ),
      legend.key = element_rect(
        color = "black",             # 图例键边框颜色
        linewidth = 0.3              # 图例键边框线宽
      ),
      legend.key.size = unit(0.6, "cm"),  # 图例键大小
      legend.key.height = unit(0.6, "cm"), # 图例键高度
      legend.key.width = unit(0.6, "cm"),  # 图例键宽度
      legend.spacing.x = unit(0.3, "cm"),  # 图例项水平间距
      legend.margin = margin(t = 0, b = 5, l = 5, r = 5),  # 图例外边距
      legend.box.margin = margin(t = 0, b = 0),  # 图例框边距
      
      # X轴标题设置 - 修改：显示X轴标题
      axis.title.x = element_text(   # 原为element_blank()
        size = 11,                   # X轴标签字体大小
        face = "bold",               # X轴标签字体加粗
        margin = margin(t = 5)       # X轴标签上边距
      ),
      
      # Y轴标题设置 - 修改：显示Y轴标题
      axis.title.y = element_text(   # 原为element_blank()
        size = 11,                   # Y轴标签字体大小
        face = "bold",               # Y轴标签字体加粗
        margin = margin(r = 5)       # Y轴标签右边距
      ),
      
      # 坐标轴刻度标签设置
      axis.text = element_text(
        size = 10,                   # 刻度标签字体大小
        color = "black"              # 刻度标签颜色
      ),
      
      # 坐标轴轴线设置
      axis.line = element_line(
        color = "black",             # 轴线颜色为黑色
        linewidth = 0.5              # 轴线线宽
      ),
      
      # 坐标轴刻度线设置
      axis.ticks = element_line(
        color = "black",             # 刻度线颜色为黑色
        linewidth = 0.3              # 刻度线线宽
      ),
      
      # 坐标轴刻度线长度设置
      axis.ticks.length = unit(0.1, "cm"),  # 刻度线长度设为0.1厘米
      
      # 网格线设置
      panel.grid.major = element_line(color = "grey90"),  # 主网格线颜色
      panel.grid.minor = element_blank()                  # 移除次网格线
    )
}

# 绘制所有6个图形（修正后无警告）
# 注意：每个变量分别绘制Syn组和Drought组的分布对比
p1 <- plot_overlap_hist(data_Syn$PH_Syn, data_Drought$PH, "PH_Syn", "PH", "PH Comparison")
p2 <- plot_overlap_hist(data_Syn$RL_Syn, data_Drought$RL, "RL_Syn", "RL", "RL Comparison")
p3 <- plot_overlap_hist(data_Syn$AFW_Syn, data_Drought$AFW, "AFW_Syn", "AFW", "AFW Comparison")
p4 <- plot_overlap_hist(data_Syn$BFW_Syn, data_Drought$BFW, "BFW_Syn", "BFW", "BFW Comparison")
p5 <- plot_overlap_hist(data_Syn$ADW_Syn, data_Drought$ADW, "ADW_Syn", "ADW", "ADW Comparison")
p6 <- plot_overlap_hist(data_Syn$BDW_Syn, data_Drought$BDW, "BDW_Syn", "BDW", "BDW Comparison")

# 排列所有图形（2行×3列）
# 使用grid.arrange将6个图形排列成2行3列的网格布局
combined_plot <- grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)

# 显示图形
print(combined_plot)



# 方法1：导出为PDF（矢量格式，适合出版和打印）
# PDF是矢量格式，无限放大不失真，文件较小
cat("正在导出PDF文件...\n")
pdf("./8_双组直方图/Syn_vs_Drought_Comparison-0.pdf",  # 文件名
    width = 5,      # 宽度：12英寸（约30.5厘米）
    height = 6,      # 高度：8英寸（约20.3厘米）
    paper = "A4")    # 纸张大小：A4

# 绘制图形到PDF设备
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)

# 关闭PDF设备，保存文件
dev.off()
cat("PDF文件已保存为: Syn_vs_Drought_Comparison-0.pdf\n\n")

# 方法2：导出为PNG（位图格式，适合网页和演示）
# PNG是位图格式，支持透明背景，适合网页使用
cat("正在导出PNG文件...\n")
png("./8_双组直方图/Syn_vs_Drought_Comparison-0.png",  # 文件名
    width = 2400,     # 宽度：2400像素（高分辨率）
    height = 1600,    # 高度：1600像素（高分辨率）
    res = 300,        # 分辨率：300 DPI（出版质量）
    pointsize = 12)   # 基础字体大小

# 绘制图形到PNG设备
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)

# 关闭PNG设备，保存文件
dev.off()
cat("PNG文件已保存为: Syn_vs_Drought_Comparison-0.png\n\n")







#### Figure 1E ######



# 清理R环境：移除所有对象+释放内存（确保脚本可重复运行）
rm(list = ls())
gc()

library(dplyr)
library(ggplot2)
library(tidyr)

# 设置工作目录（需根据实际路径修改）
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/4_WAZ")

# 读取数据（制表符分隔，含表头）
data <- read.table("./9_D面积图/data.txt", header = T, sep = "\t")


# 计算两组数的 spearman 相关性
cor_res <- cor.test(data$D_Syn, data$D_PEG, method = "spearman")
rho_val <- cor_res$estimate
p_val <- cor_res$p.value

# 格式化文本输出，P值极小时用 "< 0.001" 表示
if (p_val < 0.001) {
  p_label <- "p < 0.001"
} else {
  # 如果P值较大，保留三位小数
  p_label <- paste0("p = ", sprintf("%.3f", p_val))
}


anno_text <- sprintf("Spearman r = %.2f\n%s", rho_val, p_label)

data_plot <- data %>%
  arrange(D_PEG) %>%
  mutate(Sample = factor(Sample, levels = Sample))

# 转为长格式
df_long <- data_plot %>%
  pivot_longer(cols = c(D_Syn, D_PEG),
               names_to = "Variable",
               values_to = "Value") %>%
  mutate(Variable = factor(Variable, levels = c("D_Syn","D_PEG")))


# 获取数据最大值，用于确定注释文本在图中的 Y 轴高度
y_max <- max(df_long$Value, na.rm = TRUE)

# 创建双组折线图
p <- ggplot(df_long, aes(x = Sample, y = Value, 
                         group = Variable,   # 按Variable分组绘制
                         color = Variable)) + # 线条颜色按Variable区分

  geom_line(linewidth = 0.8, alpha = 0.8) +

  scale_color_manual(values = c("D_Syn" = "#FFB42C", "D_PEG" = "#4DBAD6")) +

  annotate("text", x = 1, y = y_max * 0.95, label = anno_text, 
           hjust = 0, vjust = 1, size = 4, color = "black", fontface = "bold") +
  
  # 4. 设置坐标轴标签
  labs(x = NULL, y = "D Value") +

  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.10))) +
  
  # 6. 设置图形主题
  theme_bw(base_size = 14) +
  
  # 7. 自定义主题细节
  theme(
    # x轴文本和刻度线隐藏（适合大样本量）
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    # 【新增】移除竖直（X轴方向）网格线，保留水平（Y轴方向）网格线
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # 图例位置
    legend.position = "top",
    # 移除图例标题，使其更清爽
    legend.title = element_blank()
  )

# 导出
ggsave("./9_D面积图/plot_line.png", plot = p, width = 3.5, height = 2.8, dpi = 300)
ggsave("./9_D面积图/plot_line.pdf", plot = p, width = 3.5, height = 2.8)


#### Figure 1F ####


# 清理R环境：移除所有对象+释放内存（确保脚本可重复运行）
rm(list = ls())
gc()

# 加载核心数据处理包
library(tidyverse)
library(ggplot2)

# 设置工作目录（请确保路径正确）
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/4_WAZ")

# 定义双阈值筛选标准
FC_UP_THRESHOLD <- 1.2       # 上调倍数阈值（> 1.2倍）
FC_DOWN_THRESHOLD <- 1/1.2   # 下调倍数阈值（< 0.833倍）
P_SIGNIFICANT <- 0.05        # 显著性 P 值阈值
P_MARGINAL <- 0.1            # 边缘显著性（趋势） P 值阈值


# 强制将性状列转换为数值型，非数字字符会被自动转为 NA
data_CK <- read.table("./13_性状/data_CK.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) %>%
  mutate(Group = "CK") %>%
  mutate(across(c(PH, RL, SFW, RFW, SDW, RDW), as.numeric))

data_Syn <- read.table("./13_性状/data_Syn.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE) %>%
  mutate(Group = "Syn") %>%
  mutate(across(c(PH, RL, SFW, RFW, SDW, RDW), as.numeric))

# 合并两组数据并转为长格式，剔除 NA 值
data_all <- bind_rows(data_CK, data_Syn) %>%
  pivot_longer(
    cols = c(PH, RL, SFW, RFW, SDW, RDW), 
    names_to = "Trait", 
    values_to = "Value"
  ) %>%
  filter(!is.na(Value))


data_filtered <- data_all %>%
  group_by(Accession, Material, Trait) %>%
  filter(sum(Group == "CK") >= 3 & sum(Group == "Syn") >= 3) %>%
  ungroup()


stat_results <- data_filtered %>%
  group_by(Accession, Material, Trait) %>%
  summarise(
    N_CK = sum(Group == "CK"),
    N_Syn = sum(Group == "Syn"),
    Mean_CK = mean(Value[Group == "CK"]),
    Mean_Syn = mean(Value[Group == "Syn"]),
    # 捕获错误的安全 t-test
    p_value = tryCatch({
      t.test(Value ~ Group, var.equal = FALSE)$p.value
    }, error = function(e) NA),
    .groups = "drop"
  ) %>%
  mutate(
    FC = Mean_Syn / Mean_CK,
    Log2FC = log2(FC),
    Status = case_when(
      is.na(p_value) ~ "Data Error",
      p_value < P_SIGNIFICANT & FC > FC_UP_THRESHOLD ~ "Up (显著上调)",
      p_value < P_SIGNIFICANT & FC < FC_DOWN_THRESHOLD ~ "Down (显著下调)",
      p_value >= P_SIGNIFICANT & p_value < P_MARGINAL & FC > FC_UP_THRESHOLD ~ "Trend Up (边缘上调)",
      p_value >= P_SIGNIFICANT & p_value < P_MARGINAL & FC < FC_DOWN_THRESHOLD ~ "Trend Down (边缘下调)",
      TRUE ~ "ns (不显著)"
    )
  )


# 定义分组
Suppressed <- c("47", "53")
Enhanced   <- c("22", "144")
Unchanged  <- c("1", "G63")
target_acc <- c(Enhanced, Unchanged, Suppressed)

# 指定 X 轴（Accession）和分面（Trait）的画图顺序
factor_acc <- target_acc
factor_trait <- c("PH", "RL", "SFW", "RFW", "SDW", "RDW")

# 4.1 提取绘图用原始散点数据
raw_plot_data <- data_filtered %>%
  filter(Accession %in% target_acc) %>%
  mutate(
    Accession = factor(Accession, levels = factor_acc),
    Trait = factor(Trait, levels = factor_trait),
    Group = factor(Group, levels = c("CK", "Syn"))
  )

# 4.2 计算画图用的基础均值与标准误 (SE)
plot_data <- raw_plot_data %>%
  group_by(Accession, Trait, Group) %>%
  summarise(
    Mean = mean(Value),
    SD = sd(Value),
    N = n(),
    SE = SD / sqrt(N),
    .groups = "drop"
  )

# 4.3 计算每个 Accession 在各 Trait 中的绝对最高点 (防止星号被散点遮挡)
y_max_data <- raw_plot_data %>%
  group_by(Accession, Trait) %>%
  summarise(Max_Raw = max(Value, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    plot_data %>% 
      group_by(Accession, Trait) %>% 
      summarise(Max_Bar = max(Mean + SE, na.rm = TRUE), .groups = "drop"),
    by = c("Accession", "Trait")
  ) %>%
  # 取原始散点最大值和误差棒最大值中的更高者
  mutate(Max_Y = pmax(Max_Raw, Max_Bar)) %>%
  select(Accession, Trait, Max_Y)

# 4.4 提取显著性打星信息，并确定星号位置
plot_stats <- stat_results %>%
  filter(Accession %in% target_acc) %>%
  mutate(
    # 按 P 值严格打星
    Sig_Label = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    # 脱离 Status 的倍数限制，直接根据均值大小上色
    Sig_Color = case_when(
      Sig_Label != "ns" & Mean_Syn > Mean_CK ~ "#D73027",  # 只要显著且均值大于CK，即标红
      Sig_Label != "ns" & Mean_Syn < Mean_CK ~ "#4575B4",  # 只要显著且均值小于CK，即标蓝
      TRUE                                   ~ "gray50"    # 不显著(ns)一律标灰
    ),
    Accession = factor(Accession, levels = factor_acc),
    Trait = factor(Trait, levels = factor_trait)
  ) %>%
  left_join(y_max_data, by = c("Accession", "Trait"))



group_colors <- c("CK" = "#78C3ED", "Syn" = "#E98FBD")

p_bar <- ggplot(plot_data, aes(x = Accession, y = Mean, fill = Group)) +
  # ⭐ 新增：底层背景色块层 (必须放在 geom_col 之前，确保其位于最下层)
  # X 轴坐标 0.5-2.5 对应第 1、2 个品系 (Enhanced)
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, fill = "#FEE0D2", alpha = 0.7) + # 浅暖红
  # X 轴坐标 2.5-4.5 对应第 3、4 个品系 (Unchanged)
  annotate("rect", xmin = 2.5, xmax = 4.5, ymin = -Inf, ymax = Inf, fill = "gray85", alpha = 0.7) +  # 浅灰
  # X 轴坐标 4.5-6.5 对应第 5、6 个品系 (Suppressed)
  annotate("rect", xmin = 4.5, xmax = 6.5, ymin = -Inf, ymax = Inf, fill = "#DEEBF9", alpha = 0.7) + # 浅冷蓝
  
  # 1. 柱形图层
  geom_col(position = position_dodge(width = 0.8), width = 0.7, color = "black", linewidth = 0.3) +
  
  # 2. 误差棒层 (SE)
  geom_errorbar(aes(ymin = Mean, ymax = Mean + SE), 
                position = position_dodge(width = 0.8), width = 0.25, linewidth = 0.5) +
  
  # 3. 散点图层：跟随分组防重叠抖动
  geom_point(data = raw_plot_data, 
             aes(x = Accession, y = Value, group = Group), 
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.8),
             size = 0.4, alpha = 0.6, color = "gray60", inherit.aes = FALSE) +
  
  # 4. 显著性星号层：基于刚刚算出的绝对最高点 Max_Y 向上悬浮 5%
  geom_text(data = plot_stats,
            aes(x = Accession, y = Max_Y + Max_Y * 0.02, label = Sig_Label, color = Sig_Color),
            inherit.aes = FALSE, size = 3, fontface = "bold", vjust = 0) +
  
  # 颜色映射注入
  scale_fill_manual(values = group_colors) +
  scale_color_identity() + 
  
  # Y 轴上方扩充15%空白，防止星号被顶掉
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
  
  # 分面，按照 6 个指标切分，各自独立的 Y 轴范围
  facet_wrap(~ Trait, scales = "free_y", ncol = 2) +
  
  labs(title = "Phenotypic Responses to Syn Inoculation",
       x = "Wheat Accessions (Enhanced | Unchanged | Suppressed)",
       y = "Trait Mean Values (+SE & Raw Data)") +
  
  # 主题优化
  theme_bw() +
  theme(
    # 【修改1】调整分面标题内部的上下边距（t=顶部距离，b=底部距离）
    strip.text = element_text(
      size = 9, 
      face = "bold", 
      margin = margin(t = 1.5, b = 1.5) # 根据需要调大或调小数值
    ),          
    strip.background = element_rect(fill = "gray95"),              
    
    # 【修改2】调整上下排分面图之间的整体距离
    panel.spacing.y = unit(0.8, "cm"), # 控制上下行的间距
    panel.spacing.x = unit(0.5, "cm"), # （可选）控制左右列的间距
    
    axis.text.x = element_text(size = 9, color = "black"),        
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b=15)),
    legend.position = "top",
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),                          
    panel.grid.minor = element_blank()
  )

# 显示图形
print(p_bar)

# 保存高分辨率图形到本地
ggsave(filename = "./13_性状/3_Barplot_Selected_Accessions-2.pdf", plot = p_bar, width = 4.8, height = 5.8)
ggsave(filename = "./13_性状/3_Barplot_Selected_Accessions-2.png", plot = p_bar, width = 4.8, height = 5.8, dpi = 300)








####******************************************************####
#### Figure 2 ####


#### Figure 2A ####


rm(list = ls())
gc()

# 加载必要R包
library(phyloseq)
library(ggplot2)
library(ggpubr)
library(agricolae)
library(vegan)
library(ggsci)
library(tidyverse)

# 设置工作目录和输出路径
work_dir <- "D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/1_data_analysis/6_plot_bac/"
setwd(work_dir)

# 自动创建结构化输出目录
out_dir <- "./2_alpha_diversity_optimized"
if(!dir.exists(out_dir)) dir.create(out_dir)
if(!dir.exists(paste0(out_dir, "/1_curve"))) dir.create(paste0(out_dir, "/1_curve"))
if(!dir.exists(paste0(out_dir, "/2_plots"))) dir.create(paste0(out_dir, "/2_plots"))


cat("【阶段 1】读取数据并进行抽平处理...\n")

# 读取phyloseq对象 (请确保文件名一致)
ps <- readRDS("./1_data_filter/6_unnorm_Rhi_bac-alpha.rds")
if(is.null(ps)) stop("未能成功读取phyloseq对象！")

# 提取OTU矩阵准备绘制稀释曲线
set.seed(123)
otu_tab <- as(otu_table(ps), "matrix")
if(taxa_are_rows(ps)) otu_tab <- t(otu_tab)

# 1.1 绘制并保存稀释曲线
pdf(paste0(out_dir, "/1_curve/Rarefaction_Curve.pdf"), width = 10, height = 8)
rarecurve(otu_tab, step = 1000, sample = min(rowSums(otu_tab)), label = FALSE, 
          main = "Rarefaction Curves", col = "blue")
dev.off()

# 1.2 执行数据抽平
min_depth <- min(rowSums(otu_tab))
cat("最小测序深度为:", min_depth, "，以此深度进行抽平。\n")
# rngseed=123 保证结果可重复
ps_rare <- rarefy_even_depth(ps, sample.size = min_depth, rngseed = 123, verbose = FALSE)


cat("【阶段 2】基于抽平数据计算 Alpha 多样性...\n")

# 使用phyloseq内置函数一键计算基础指标
alpha_rare <- estimate_richness(ps_rare, measures = c("Observed", "Shannon", "Simpson", "Chao1", "ACE"))

# 补充计算 Simpson_D 和 Pielou 均匀度
alpha_rare$Simpson_D <- 1 - alpha_rare$Simpson
alpha_rare$Pielou <- alpha_rare$Shannon / log(alpha_rare$Observed)
alpha_rare$SampleID <- rownames(alpha_rare)

# 提取并合并元数据 (Metadata)
meta <- as(sample_data(ps_rare), "data.frame")
meta$SampleID <- rownames(meta)

# 合并多样性结果与分组信息
alpha_data <- left_join(alpha_rare, meta, by = "SampleID")

# 保存计算结果
write.csv(alpha_data, paste0(out_dir, "/AlphaDiversity_Table_Rarefied.csv"), row.names = FALSE)


cat("【阶段 3】执行ANOVA检验(BH校正)并绘图...\n")

# 定义分析的指标
diversity_metrics <- c("Chao1", "ACE", "Shannon", "Simpson", "Simpson_D", "Pielou")

# 设置X轴分组顺序（必须与你 metadata 中的 Group2 列一致）
desired_order <- c("Sensitive_CK", "Sensitive_Syn", "Tolerant_CK", "Tolerant_Syn")
alpha_data$Group2 <- factor(alpha_data$Group2, levels = desired_order)


# 3.1 统计分析函数 (ANOVA + LSD with BH校正)
perform_statistical_tests <- function(data, metric, group_var = "Group2") {
  # 动态生成公式: metric ~ group_var
  formula <- reformulate(response = metric, termlabels = group_var)
  
  # 执行单因素方差分析 (ANOVA)
  model <- aov(formula, data = data)
  
  # LSD事后检验 (此处严谨地使用了 "BH" 进行假阳性校正)
  lsd_result <- LSD.test(model, trt = group_var, alpha = 0.05, p.adj = "BH", group = TRUE)
  
  # 整理标记
  result <- data.frame(
    Group2 = rownames(lsd_result$groups),
    Mean = lsd_result$means[rownames(lsd_result$groups), 1],
    Std = lsd_result$means[rownames(lsd_result$groups), 2],
    Label = trimws(lsd_result$groups[, 2]) # 去除可能多余的空格
  )
  return(result)
}

# 3.2 可视化核心函数
create_diversity_plot <- function(data, metric, stats_result) {
  y_max <- max(data[[metric]], na.rm = TRUE) * 1.15
  drought_levels <- unique(data$Drought)
  
  # 颜色配置映射
  custom_colors <- c(
    "Tolerant" = "#4DBBD5FF", 
    "Sensitive" = "#E64B35FF"
  )
  fill_colors <- custom_colors[names(custom_colors) %in% drought_levels]
  if (length(fill_colors) == 0) fill_colors <- ggsci::pal_npg("nrc")(length(drought_levels))
  
  p <- ggplot(data, aes(x = Group2, y = !!sym(metric), fill = Drought)) +
    stat_boxplot(geom = "errorbar", width = 0.3) +
    geom_boxplot(outlier.colour = "red", outlier.size = 1, alpha = 0.9) +
    geom_jitter(position = position_jitter(0.17), size = 1, alpha = 0.6) +
    stat_compare_means(method = "kruskal.test", label.y = y_max * 0.98, label.x = 1, label = "p.format") +
    # 添加显著性字母 (脱离aes继承，避免fill报错)
    geom_text(
      data = stats_result,
      aes(x = Group2, y = Mean + Std + (y_max * 0.05), label = Label),
      inherit.aes = FALSE, 
      vjust = 0, size = 5, fontface = "bold"
    ) +
    scale_x_discrete(limits = desired_order) +
    scale_y_continuous(limits = c(0, y_max), expand = expansion(mult = c(0, 0.05))) +
    scale_fill_manual(values = fill_colors) +
    theme_bw() +
    theme(
      text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
      panel.border = element_rect(fill = NA, color = "black", size = 1),
      plot.margin = margin(t=5, r=5, b=5, l=5, unit="mm"),
      legend.position = "right"
    ) +
    labs(x = "", y = metric, fill = "Genotype") 
  
  return(p)
}

# 4. 主干批处理循环 (分析 + 绘图)
for (metric in diversity_metrics) {
  
  # 1. 直接使用完整数据（已移除异常值剔除步骤）
  analysis_data <- alpha_data
  
  # 2. ANOVA + BH 校正 LSD
  stats_result <- perform_statistical_tests(analysis_data, metric)
  write.csv(stats_result, paste0(out_dir, "/2_plots/", metric, "_ANOVA_BH_LSD.csv"), row.names = FALSE)
  
  # 3. 生成可视化
  p_plot <- create_diversity_plot(analysis_data, metric, stats_result)
  
  # 4. 保存图片
  ggsave(filename = paste0(out_dir, "/2_plots/", metric, "_alpha.pdf"), plot = p_plot, width = 5.5, height = 4)
  ggsave(filename = paste0(out_dir, "/2_plots/", metric, "_alpha.jpg"), plot = p_plot, width = 5.5, height = 4, dpi = 300)
  
  cat("✔ 已完成指标:", metric, "\n")
}

cat("\n🎯 分析全部完成！结果已保存至:", out_dir, "\n")


#### Figure 2B ####


rm(list = ls())
gc()




# 1. 工作目录设置与文件夹创建
main_dir <- "D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/1_data_analysis/6_plot_bac/"
setwd(main_dir)
# setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/1_data_analysis/6_plot_bac/")
# dir.create("3_beta_diversity")
# dir.create("3_beta_diversity/0_data/")
# 
data_dir <-  paste0(main_dir, "./3_beta_diversity/")
# 创建必要的目录结构
dir.create(file.path(data_dir, "0_data"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(data_dir, "1_permanova"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(data_dir, "2_plots"), recursive = TRUE, showWarnings = FALSE)


output_dir <- "./3_beta_diversity/0_data/"
# if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


setwd(data_dir)

# 设置日志文件

sink()
log_file <- file.path(data_dir, "analysis_log-2.txt")
if(file.exists(log_file)) file.remove(log_file)
# 开始记录日志
sink(log_file, append = TRUE, split = TRUE)

# 记录分析开始时间
start_time <- Sys.time()
message("\n>>> 分析开始时间: ", start_time, "\n")


# 2 加载必要包


message("加载R包...")

required_packages <- c("phyloseq", "vegan", "ggplot2", "ape", "dplyr", 
                       "tidyr", "ggpubr", "RColorBrewer", "patchwork")

invisible(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  message("已加载: ", pkg)
}))





# 3 数据准备 


#数据准备函数

load_data <- function() {
  message("\n加载数据...")
  
  # 1. 加载phyloseq对象
  ps_path <- file.path(data_dir, "../1_data_filter/4_CSS_norm_Rhi_bac.rds")
  if(!file.exists(ps_path)) stop("错误: Phyloseq对象文件不存在 - ", ps_path)
  
  message("正在读取phyloseq对象...")
  ps <- readRDS(ps_path)
  
  # 数据质量检查
  message("\n数据概览:")
  message("样本数量: ", nsamples(ps))
  message("OTU数量: ", ntaxa(ps))
  
  if(ntaxa(ps) < 10) warning("警告: OTU数量较少(", ntaxa(ps), ")，可能影响分析结果")
  
  # 2. 计算Bray-Curtis距离
  message("\n计算Bray-Curtis距离矩阵...")
  bray_dist <- phyloseq::distance(ps, method = "bray")
  
  # 3. 加载元数据
  meta_path <- file.path(data_dir, "../1_data_filter/5_filtered_sample_data_Rhi_bac.csv")
  if(!file.exists(meta_path)) stop("错误: 元数据文件不存在 - ", meta_path)
  
  message("正在读取元数据...")
  metadata <- tryCatch({
    meta <- read.table(meta_path, header = TRUE, row.names = 1, 
                       sep = ",", stringsAsFactors = TRUE, check.names = FALSE)
    
    # 样本匹配检查
    if(!all(labels(bray_dist) %in% rownames(meta))) {
      stop("错误: 距离矩阵和元数据的样本不匹配")
    }
    meta <- meta[labels(bray_dist), , drop = FALSE]
    
    # 检查分组变量
    group_vars <- c("Group2", "Drought", "Treatment")
    
    # 验证分组变量是否存在
    available_vars <- group_vars[group_vars %in% colnames(meta)]
    missing_vars <- setdiff(group_vars, colnames(meta))
    
    if(length(missing_vars) > 0) {
      warning("警告: 以下分组变量在元数据中不存在: ", paste(missing_vars, collapse = ", "))
    }
    
    # 检查NA值
    for(var in available_vars) {
      if(any(is.na(meta[[var]]))) {
        warning("警告: 分组变量 '", var, "' 包含NA值")
      }
    }
    
    list(
      metadata = meta,
      available_vars = available_vars
    )
  }, error = function(e) {
    stop("元数据加载失败: ", e$message)
  })
  
  message("\n可用的分组变量: ", paste(metadata$available_vars, collapse = ", "))
  
  return(list(
    dist_obj = bray_dist,
    metadata = metadata$metadata,
    group_vars = metadata$available_vars,
    phyloseq = ps
  ))
}



data <- tryCatch({
  load_data()
}, error = function(e) {
  stop("数据加载失败: ", e$message)
})




# 自定义配色方案函数（放在脚本开头部分）
get_custom_palette <- function(n) {
  # 确保n是数值类型
  if (!is.numeric(n)) stop("n must be a number")
  
  # 基础颜色（来自Paired调色板的12色）
  base_colors <- c(
    "#FCBB44", "#F1766D", "#7A70B5", "#839DD1", "#FB9A99", "#E31A1C",
    "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#FFFF99", "#B15928"
  )
  
  # 扩展颜色（补充8色，总计20色）
  extended_colors <- c(
    "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", 
    "#FDB462", "#B3DE69", "#FCCDE5", "#D9D9D9", "#BC80BD"
  )
  
  # 动态生成方案
  if (n <= 12) {
    return(base_colors[1:n])
  } else if (n <= 20) {
    return(c(base_colors, extended_colors)[1:n])
  } else {
    # 超过20组时使用viridis（比rainbow更好的选择）
    warning(paste(n, "groups detected - using viridis palette which provides better visual discrimination"))
    return(viridis::viridis(n))
  }
}



# 分析函数 

run_permanova <- function(dist_obj, metadata, group_vars) {
  message("\n运行PERMANOVA分析...")
  #  
  #  dist_obj
  #  metadata
  #  group_vars
  #  
  # a <- data$metadata
  #  b <- data$dist_obj
  
  # 定义分析模型
  models <- list(
    # 单变量模型
    "Single_Drought" = dist_obj ~ Drought,
    "Single_Treatment" = dist_obj ~ Treatment,
    "Single_Group2" = dist_obj ~ Group2,
    
    # 主效应组合模型
    "Main_effects" = dist_obj ~Drought + Treatment,
    
    # 目的：检验所有变量的 独立（加性）效应，忽略交互作用。
    # 输出：每个变量解释的变异比例（R²）和p值。
    
    # 交互作用模型
    "Drought_Treatment_interaction" = dist_obj ~ Drought * Treatment,
    
    # # 特殊组合变量交互
    "Drought_Group2_interaction" = dist_obj ~ Drought * Group2,
    "Treatment_Group2_interaction" = dist_obj ~ Treatment * Group2
  )
  
  # 运行所有模型
  results <- list()
  summary_list <- list()
  
  for(model_name in names(models)) {
    formula <- models[[model_name]]
    required_vars <- all.vars(formula)[-1]
    
    message("  |- 运行模型: ", model_name)
    
    # 检查变量是否存在
    missing_vars <- setdiff(required_vars, colnames(metadata))
    if(length(missing_vars) > 0) {
      warning("    |- 跳过模型 ", model_name, " - 缺少变量: ", paste(missing_vars, collapse = ", "))
      next
    }
    
    # 检查NA值
    na_vars <- sapply(required_vars, function(var) any(is.na(metadata[[var]])))
    if(any(na_vars)) {
      warning("    |- 模型 ", model_name, " 变量包含NA值: ", 
              paste(names(na_vars)[na_vars], collapse = ", "))
    }
    
    # 执行PERMANOVA
    res <- tryCatch({
      adonis2(
        formula,
        data = metadata,
        permutations = 999,
        by = "terms",
        parallel = getOption("mc.cores", 1)
      )
    }, error = function(e) {
      warning("    |- 模型 ", model_name, " 运行失败: ", e$message)
      NULL
    })
    
    if(is.null(res)) next
    
    # 保存详细结果
    write.csv(
      as.data.frame(res),
      file.path(data_dir, "./1_permanova", paste0(model_name, "_results.csv")),
      row.names = TRUE
    )
    
    # 提取关键结果
    results[[model_name]] <- res
    
    # 准备汇总结果
    summary_df <- data.frame(
      Model = model_name,
      Formula = paste(deparse(formula), collapse = ""),
      Term = rownames(res),
      Df = res$Df,
      R2 = round(res$R2, 4),
      F = round(res$F, 2),
      P_value = ifelse(res$`Pr(>F)` < 0.001, "<0.001", round(res$`Pr(>F)`, 4)),
      stringsAsFactors = FALSE
    )
    
    summary_list[[model_name]] <- summary_df
  }
  
  # 合并所有汇总结果
  if(length(summary_list) > 0) {
    summary_df <- do.call(rbind, summary_list)
    rownames(summary_df) <- NULL
    
    write.csv(
      summary_df,
      file.path(data_dir, "./1_permanova/permanova_summary.csv"),
      row.names = FALSE
    )
    
    # 保存显著结果
    sig_results <- summary_df[summary_df$P_value != "NA" & 
                                (summary_df$P_value == "<0.001" | 
                                   as.numeric(summary_df$P_value) < 0.05), ]
    
    if(nrow(sig_results) > 0) {
      write.csv(
        sig_results,
        file.path(data_dir, "./1_permanova/significant_results.csv"),
        row.names = FALSE
      )
    }
  }
  
  return(list(
    all_results = results,
    summary = summary_df
  ))
}





# 3. PERMANOVA分析
permanova <- tryCatch({
  run_permanova(data$dist_obj, data$metadata, data$group_vars)
}, error = function(e) {
  warning("PERMANOVA分析失败: ", e$message)
  NULL
})



# 3.3 进行PcOA分析 

#  PCoA分析函数
run_pcoa <- function(dist_obj, metadata, group, title_suffix = "") {
  message("  |- 运行PCoA分析 (分组变量: ", group, ")")
  
  # 执行PCoA分析
  pcoa <- cmdscale(dist_obj, k = 3, eig = TRUE)
  
  # 准备结果数据框
  pcoa_points <- as.data.frame(pcoa$points)
  colnames(pcoa_points) <- paste0("PCoA", 1:3)
  pcoa_points <- cbind(pcoa_points, metadata)
  
  # 计算方差解释度
  eig_percent <- round(100 * pcoa$eig[pcoa$eig > 0] / sum(pcoa$eig[pcoa$eig > 0]), 2)
  
  # 创建更友好的分组名称
  group_name <- gsub("([[:upper:]])", " \\1", group)
  
  # 获取分组数量
  n_groups <- length(unique(pcoa_points[[group]]))
  
  # 使用自定义配色方案
  custom_colors <- get_custom_palette(n_groups)
  
  # 可视化
  p <- ggplot(pcoa_points, aes(x = PCoA1, y = PCoA2, color = .data[[group]])) +
    geom_point(size = 1.5, alpha = 0.8) +
    stat_ellipse(level = 0.95, linewidth = 0.8) +
    scale_color_manual(values = custom_colors, name = group_name) +
    labs(
      x = sprintf("PCoA1 (%.1f%%)", eig_percent[1]),
      y = sprintf("PCoA2 (%.1f%%)", eig_percent[2]),
      title = paste("Bray-Curtis PCoA:", group_name)
    ) +
    theme_bw(base_size = 18) +
    theme(
      panel.grid = element_blank(),
      legend.position = "right",
      aspect.ratio = 1,
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
  
  # 添加组间中心点
  centroids <- pcoa_points %>%
    group_by(.data[[group]]) %>%
    summarise(PCoA1 = mean(PCoA1), PCoA2 = mean(PCoA2))
  
  p <- p + geom_point(data = centroids, size = 2.0, shape = 18, show.legend = FALSE)
  
  # 保存结果
  plot_name <- paste0("PCoA_bray_", group)
  
  ggsave(
    file.path(data_dir, "2_plots", paste0(plot_name, ".pdf")), 
    plot = p, 
    width = 5.5, 
    height = 4.5
  )
  
  ggsave(
    file.path(data_dir, "2_plots", paste0(plot_name, ".png")), 
    plot = p, 
    width = 5.5, 
    height = 4.5, 
    dpi = 300
  )
  
  return(list(
    plot = p,
    points = pcoa_points,
    eig = eig_percent
  ))
}



# 执行分析
# 对每个分组变量运行PCoA和NMDS

for(group in data$group_vars) {
  message("\n>>> 分析分组变量: ", group)
  
  # PCoA分析
  pcoa_results <- tryCatch({
    run_pcoa(data$dist_obj, data$metadata, group)
  }, error = function(e) {
    warning("PCoA分析失败 (", group, "): ", e$message)
    NULL
  })
  
}




#### Figure 2C####


rm(list = ls())
gc()
options(scipen = 0)  # 通常100就足够了


main_dir <- "D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/1_data_analysis/6_plot_bac/"
setwd(main_dir)

# 
output_dir <- "./4_ASV_abundance/1_Single-exp/"
#recursive=TRUE允许创建多级目录
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)



# 加载必要的R包
library(dplyr)        # 数据清洗和转换
library(tidyr)        # 数据重塑和整理
library(ggplot2)      # 数据可视化
library(stringr)      # 字符串处理
library(tibble)       # 数据框增强功能
library(magrittr)     # 提供管道操作符
library(ggpubr)


# (1) 各样本属水平丰度提取


ps <- readRDS("./1_data_filter/4_TSS_norm_Rhi_bac.rds")


# 数据提取与格式化 
# 提取OTU表（ASV丰度矩阵）
# - taxa_are_rows = TRUE 表示行名为ASV编号
df_bac <- as.data.frame(otu_table(ps, taxa_are_rows = TRUE))
message("ASV表维度: ", nrow(df_bac), "个ASV × ", ncol(df_bac), "个样本")

# 提取样本元数据
Sample <- data.frame(sample_data(ps))
message("分组变量: ", paste(colnames(Sample), collapse = ", "))

# 提取分类信息
tax0 <- data.frame(tax_table(ps))
# 确保有规范的分类等级名
if(!all(c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species") %in% colnames(tax0))) {
  colnames(tax0)[1:7] <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
}


tax <- tax0 %>% 
  mutate(Genus = coalesce(Genus, "g__Unclassified"))  # 处理NA值

# 查看数据结构
head(df_bac)
head(Sample)
head(tax)

# 添加公共合并列
Sample$Sample <- rownames(Sample)
tax$ASV <- rownames(tax)
tax <- tax[,-8]   # 删除 confidence列

df_bac$ASV <- rownames(df_bac)


# 将细菌ASV丰度表(df_bac)与样本信息(Sample)、分类信息(tax)合并为长格式数据框
df_combined <- merge(df_bac, tax, by = "ASV", all = T)
df_combined_1 <- df_combined[,c(57,2:51)]
genus <- df_combined_1$Genus

# 按菌属聚合丰度
exp_genus <- aggregate(
  df_combined_1[ , -1],  # 输入数据（排除最后一列）
  by = list(Genus = df_combined_1$Genus),                          # 按 genus 分组
  sum                                        # 对每组求和
)

rownames(exp_genus) <- gsub("g__", "", exp_genus$Genus)

exp_genus_1 <- exp_genus[,-1]




candidate <- c("Pseudomonas", "Sphingomonas", "Variovorax", "Stenotrophomonas" )

for (i in 1:length(candidate)){
  #  i <- 2
  
  keyword <- candidate[i]
  
  K_exp <- as.data.frame(t(exp_genus_1[keyword,]))
  K_exp$Sample <- rownames(K_exp)
  
  K_Exp <- merge(K_exp, Sample, by = "Sample", all = T)
  
  drew_data <- K_Exp[,c(1,2,3,5,10)]
  
  

  # 设置因子顺序
  drew_data$Drought <- factor(drew_data$Drought, levels = c("Sensitive", "Tolerant"))
  drew_data$Treatment <- factor(drew_data$Treatment, levels = c("CK", "Syn"))
  
  # 1. 计算详细的统计结果
  stats_summary <- drew_data %>%
    group_by(Drought, Treatment) %>%
    summarise(
      Mean = mean(.data[[keyword]]),
      SD = sd(.data[[keyword]]),
      SE = sd(.data[[keyword]]) / sqrt(n()),
      N = n(),
      .groups = 'drop'
    )
  
  # 2. 计算每组内CK vs Syn的p值
  p_values <- drew_data %>%
    group_by(Drought) %>%
    summarise(
      p_value = wilcox.test(.data[[keyword]] ~ Treatment)$p.value,
      p_value_t = t.test(.data[[keyword]] ~ Treatment)$p.value,  # t检验作为参考
      .groups = 'drop'
    ) %>%
    mutate(
      significance = case_when(
        p_value < 0.001 ~ "***",
        p_value < 0.01 ~ "**",
        p_value < 0.05 ~ "*",
        TRUE ~ "ns"
      ),
      # 计算y位置
      y_pos = drew_data %>%
        group_by(Drought) %>%
        summarise(max_val = max(.data[[keyword]])) %>%
        mutate(y_pos = max_val * 1.3) %>%
        pull(y_pos)
    )
  
  # 打印p值
  print("Wilcoxon检验结果：")
  print(p_values)
  
  # 设置因子顺序
  drew_data$Drought <- factor(drew_data$Drought, levels = c("Sensitive", "Tolerant"))
  drew_data$Treatment <- factor(drew_data$Treatment, levels = c("CK", "Syn"))
  
  
  # 先计算y轴范围（使用基础R语法）
  y_max <- max(drew_data[[keyword]], na.rm = TRUE)  # 使用双中括号
  y_min <- min(drew_data[[keyword]], na.rm = TRUE)  # 最小值（如果需要）
  
  # 绘制修正后的图形
  p4_corrected <- ggplot() +
    # 柱状图显示均值
    geom_bar(data = stats_summary, 
             aes(x = Drought, y = Mean, fill = Treatment),
             stat = "identity", position = position_dodge(0.9), width = 0.85, alpha = 0.8) +
    # 误差线 - 修正：移除对.data[[keyword]]的引用
    geom_errorbar(data = stats_summary,
                  aes(x = Drought, ymin = Mean - SE, ymax = Mean + SE, 
                      group = Treatment),
                  position = position_dodge(0.9), width = 0.2) +
    # 添加原始数据点 - 使用drew_data
    geom_jitter(data = drew_data,
                aes(x = Drought, y = .data[[keyword]], color = Treatment),
                position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.9), 
                size = 1.2, alpha = 0.5) +
    # 颜色设置
    scale_fill_manual(values = c("CK" = "#1b9e77", "Syn" = "#d95f02")) +
    scale_color_manual(values = c("CK" = "#1b9e77", "Syn" = "#d95f02")) +
    ylim(0, y_max * 1.2) +  # 根据您的数据调整上限
    # 添加显著性标记
    geom_text(data = p_values,
              aes(x = Drought, y = y_pos *0.8, label = significance),
              inherit.aes = FALSE, size = 5, fontface = "bold") +
    # 添加横线连接CK和Syn
    geom_segment(data = p_values,
                 aes(x = as.numeric(Drought) - 0.2, xend = as.numeric(Drought) + 0.2,
                     y = y_pos * 0.8, yend = y_pos * 0.8),
                 inherit.aes = FALSE, size = 0.5) +
    labs(title = paste0(keyword, " Abundance"),
         subtitle = " * p<0.05, ** p<0.01, *** p<0.001",
         x = "Drought Tolerance", 
         y = "Abundance",
         fill = "Treatment",
         color = "Treatment") +
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      legend.position = "top",
      legend.box = "horizontal"
    )
  
  # 显示图形
  print(p4_corrected)
  
  
  
  # 1. 导出为PDF
  ggsave(
    filename = paste0(output_dir,keyword, "_abundance_plot_TSS.pdf"),  # 文件名
    plot = p4_corrected,                           # 图形对象
    width = 2.5,                                      # 宽度（英寸）
    height = 3,                                     # 高度（英寸）
    units = "in",                                   # 单位
    dpi = 300                                       # 分辨率
  )
  
  # 2. 导出为JPEG
  ggsave(
    filename = paste0(output_dir,keyword, "_abundance_plot_TSS.jpeg"),   # 文件名
    plot = p4_corrected,                            # 图形对象
    width = 2.5,                                       # 宽度（英寸）
    height = 3,                                      # 高度（英寸）
    units = "in",                                    # 单位
    dpi = 300,                                       # 分辨率
    quality = 90                                     # JPEG质量（1-100）
  )
  
  
  
  
  
}



#### Figure 2D####

rm(list = ls())
gc()




setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/1_data_analysis/6_plot_bac/5_LEfSe/") 


S_LEFSE <- read.table("./1_sensitive/0_sensitive_lefse_all.txt", header = T, sep = "\t")
S_LEFSE_LDA2 <- S_LEFSE[S_LEFSE$LDA > 3 & S_LEFSE$P.adj < 0.05,]
id_S <- data.frame(S_LEFSE_LDA2$Taxa, drop = F)
colnames(id_S) <- c("taxa","drop")

T_LEFSE <- read.table("./1_tolerant/0_tolerant_lefse_all.txt", header = T, sep = "\t")
T_LEFSE_LDA2 <- T_LEFSE[T_LEFSE$LDA > 3 & T_LEFSE$P.adj < 0.05,]
id_T <- data.frame(T_LEFSE_LDA2$Taxa, drop = F)
colnames(id_T) <- c("taxa","drop")

id <- rbind(id_S, id_T)

id_all <- unique(id$taxa)

S_LEFSE_drew0 <- S_LEFSE[S_LEFSE$Taxa %in% id_all,] 
T_LEFSE_drew0 <- T_LEFSE[T_LEFSE$Taxa %in% id_all,]





data0 <- rbind(S_LEFSE_drew0, T_LEFSE_drew0)
data0$names <- c(1:nrow(data0))
data <- data0[,c(2,1,4,5,8)]



library(ggplot2)
library(dplyr)
library(tidyr)

# 1. 数据预处理
# 对原始数据进行清洗和转换，创建绘图所需的变量
data_processed <- data %>%
  mutate(
    # 提取Taxa短名称：从管道分隔的字符串中提取最后一部分，并替换下划线为空格
    Taxa_short = sapply(strsplit(Taxa, "\\|"), function(x) {
      last_part <- tail(x, 1)  # 获取最后一部分分类信息
      if(last_part == "") return("Unknown")  # 处理空值情况
      gsub("_", " ", last_part)  # 将下划线替换为空格提高可读性
    }),
    
    # 主要分组：直接使用Comparison列作为主要分组变量
    Main_group = Comparison,
    
    # 亚组（CK或Syn）：根据组名后缀判断
    Subgroup = case_when(
      grepl("_CK$", Group) ~ "CK",   # 以_CK结尾的组
      grepl("_Syn$", Group) ~ "Syn", # 以_Syn结尾的组
      TRUE ~ "Unknown"               # 其他情况
    ),
    
    # 显著性：将显著性标记转换为有序因子，便于后续可视化
    Significance_level = factor(
      Significance,
      levels = c("***", "**", "*", "ns"),  # 定义顺序
      labels = c("***", "**", "*", "ns")   # 保持标签不变
    )
  )

# 2. 获取所有Taxa列表：提取处理后的唯一Taxa名称
all_taxa <- unique(data_processed$Taxa_short)

# 3. 提取Tolerant组数据用于排序：筛选特定比较组的数据
tolerant_data <- data_processed %>%
  filter(Main_group == "Tolerant_CK - Tolerant_Syn")

sensitive_data <- data_processed %>%
  filter(Main_group == "Sensitive_CK - Sensitive_Syn")



# 4. 对Sensitive组排序：
# CK在前，按LDA降序排列
ck_sorted <- sensitive_data %>%
  filter(Subgroup == "CK") %>%
  arrange(desc(LDA)) %>%  # 按LDA值降序排列
  pull(Taxa_short) %>%    # 提取Taxa名称列
  unique()                # 确保唯一性

# Syn在后，按LDA降序排列
syn_sorted <- sensitive_data %>%
  filter(Subgroup == "Syn") %>%
  arrange(desc(LDA)) %>%
  pull(Taxa_short) %>%
  unique()

# 5. 合并排序：先所有CK，后所有Syn
sorted_taxa <- c(ck_sorted, syn_sorted)

# 6. 创建绘图数据 - 两个组共享同一个排序
plot_data <- data_processed %>%
  mutate(
    # 将Taxa名称转换为因子，使用排好的顺序
    Taxa_ordered = factor(Taxa_short, levels = sorted_taxa)
  ) %>%
  # 设置LDA方向：Tolerant组朝上，Sensitive组朝下
  mutate(
    LDA_directed = case_when(
      # Tolerant组：取绝对值确保为正值（朝上）
      Main_group == "Sensitive_CK - Sensitive_Syn" ~ abs(LDA),
      # Sensitive组：取负绝对值确保为负值（朝下）
      Main_group == "Tolerant_CK - Tolerant_Syn" ~ -abs(LDA),
      TRUE ~ 0  # 其他情况（理论上不应存在）
    ),
    
    # 创建分组变量用于颜色编码
    Color_group = paste(
      ifelse(Main_group == "Sensitive_CK - Sensitive_Syn",  "Sensitive", "Tolerant"),
      Subgroup,
      sep = "_"
    ),
    
    # 根据显著性设置填充颜色：不显著的统一用灰色
    Fill_color = ifelse(Significance == "ns", "ns", Color_group),
    
    # 组标签：简化分组名称
    Group_label = ifelse(Main_group == "Tolerant_CK - Tolerant_Syn", "Sensitive", "Tolerant" )
  ) %>%
  filter(!is.na(Taxa_ordered))  # 移除NA值



# 7. 颜色方案：定义各组颜色
color_scheme <- c(
  "Tolerant_CK" = "#3492B2",    # 红色
  "Tolerant_Syn" = "#073E7F",   # 蓝色
  "Sensitive_CK" = "#F49600",   # 橙色
  "Sensitive_Syn" = "#E41A1C",  # 紫色
  "ns" = "#999999"              # 灰色（不显著）
)



S_max <- max(sensitive_data$LDA)
T_max <- max(tolerant_data$LDA)



# 8. 创建图形
p <- ggplot(plot_data, aes(x = Taxa_ordered, y = LDA_directed)) +
  
  # 设置Y轴范围
  scale_y_continuous(limits = c(S_max * -1.2, T_max * 1.2),      # Y轴从-4到4
                     breaks = seq(-5, 5, by = 1.5),  # 每1个单位一个刻度
                     expand = c(0, 0)) +     # 取消轴两端扩展
  
  # 设置X轴可视范围（从1到210）
  coord_cartesian(xlim = c(0, 47)) +  # 包含所有199个样本并扩展到210
  
  
  
  # 水平参考线：零线（灰色虚线）
  geom_hline(yintercept = 0, linetype = "solid", 
             color = "black", linewidth = 0.7, alpha = 0.95) +
  
  # 新增：LDA=2和LDA=-2参考线（红色虚线，较粗）
  geom_hline(yintercept = 3, linetype = "dashed", 
             color = "red", linewidth = 0.5, alpha = 0.9) +
  geom_hline(yintercept = -3, linetype = "dashed", 
             color = "red", linewidth = 0.5, alpha = 0.9) +
  
 
  geom_segment(
    aes(xend = Taxa_ordered, y = 0, yend = LDA_directed, 
        color = Color_group, group = Group_label),
    linewidth = 0.3, alpha = 0.8
  ) +
  

  geom_point(
    aes(fill = Fill_color, size = Significance_level),
    shape = 21,                            # 21号图形是有边框的圆形
    color = "black",                       # 边框颜色（黑色）
    stroke = 0.2,                          # 边框粗细（单位：mm）
    alpha = 0.7
  ) +
  
  
  
  
  # 线段颜色映射
  scale_color_manual(
    name = "Group & Subgroup",
    values = color_scheme[1:4],
    labels = c(
      "Tolerant_CK" = "Tolerant: CK",
      "Tolerant_Syn" = "Tolerant: Syn",
      "Sensitive_CK" = "Sensitive: CK",
      "Sensitive_Syn" = "Sensitive: Syn"
    )
  ) +
  
  # 点填充颜色映射
  scale_fill_manual(
    name = "Significance",
    values = color_scheme,
    labels = c(
      "Tolerant_CK" = "Tolerant: CK (sig)",
      "Tolerant_Syn" = "Tolerant: Syn (sig)",
      "Sensitive_CK" = "Sensitive: CK (sig)",
      "Sensitive_Syn" = "Sensitive: Syn (sig)",
      "ns" = "Not significant"
    )
  ) +
  
  # 点大小映射：根据显著性级别调整
  scale_size_manual(
    name = "Significance Level",
    values = c("***" = 4.0, "**" = 3.0, "*" = 2.0, "ns" = 1)
  ) +
  
  
  
  
  # 坐标轴和标题
  labs(
    y = "LDA Score"  ) +

  annotate("text",
           x = length(sorted_taxa)/2,
           y = S_max*1.1,
           label = "Sensitive",
           hjust = 0.5, vjust = 0.5, size = 3.5, fontface = "bold",
           color = "#E41A1C", alpha = 0.7) +
  
  annotate("text",
           x = length(sorted_taxa)/2,
           y = T_max * -1.1,
           label = "Tolerant",
           hjust = 0.5, vjust = 0.5, size = 3.5, fontface = "bold",
           color = "#377EB8", alpha = 0.7) +
  
  # 主题设置
  theme_minimal(base_size = 9) +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.5),  # Y轴轴线
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5 , size = 8.5, color = "black"),  # 倾斜45度，右对齐，放大，变蓝
    axis.ticks.x = element_line(color = "black", linewidth = 0.5),                     # 刻度线变红、加粗
    
    axis.text.y = element_text(size = 8.5, color = "black"),
    axis.title = element_text(face = "bold", size = 9),
    # 新增：显示Y轴轴线和刻度
    axis.line.y = element_line(color = "black", linewidth = 0.5),  # Y轴轴线
    axis.ticks.y = element_line(color = "black", linewidth = 0.5), # Y轴刻度线
    axis.ticks.length.y = unit(2, "pt"),  # 设置刻度线长度
    
    
    # 图例设置
    legend.position = "right",
    legend.box = "vertical",
    legend.spacing.y = unit(0.2, "cm"),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 8),
    
    # 网格线设置
    panel.grid.major.y = element_line(color = "gray60", linewidth = 0.3),
    panel.grid.minor.y = element_line(color = "gray70", linewidth = 0.2),
    # panel.grid.major.x = element_blank(),
    panel.grid.major.x = element_line(color = "gray80", linewidth = 0.3),
    panel.grid.minor.x = element_blank(),
    
    # 绘图区域设置
    plot.title = element_text(face = "bold", size = 9, hjust = 0.5),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "gray40"),
    plot.margin = margin(20, 60, 20, 20)  # 上右下左边距
  )



# 9. 显示图形
print(p)


ggsave("LDA_compare-2.pdf", 
       plot = p,
       width = 9.5,  # 动态调整宽度
       height = 4,                                  # 固定高度
       device = cairo_pdf,                           # 使用cairo设备支持中文
       dpi = 300)                                    # 高分辨率

# 2. 导出PNG（位图，适合网页和演示）

ggsave("LDA_compare-2.png", 
       plot = p,
       width = 9.5,  # 动态调整宽度
       height = 4,                                  # 固定高度
       dpi = 300,                                    # 高分辨率
       bg = "white")                                 # 白色背景






####******************************************************####


#### Figure 3 ####


##### Figure 3A####


rm(list = ls())
gc()


setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/6-温室表型/3_trend/")

library(ggplot2)
library(dplyr)
library(ggpubr)
library(tidyr)

library(ggsignif)


c("Sample1", "Sample2", "line", "Bac" , "Bac2" ,"number",  "PH", "RL",  "TN", "LL", "up_FW","up_DW","down_FW", "down_DW", "FW","DW" )     



drew_data <- read.table("./data0.txt", header = T, sep = "\t")



library(dplyr)
library(tidyr)
library(ggplot2)

# 1. 定义需要循环绘图的目标列
target_vars <- c("PH", "RL", "TN", "LL", "up_FW", "up_DW", "down_FW", "down_DW", "FW", "DW")

# 2. 确保因子顺序一致（保证画图时 X 轴的顺序以及折线的分组正确）
# 如果需要特定顺序，可以在这里修改 levels。默认将 CK 放在最前面。
bac_unique <- unique(drew_data$Bac)
bac_levels <- c("CK", "Syn", setdiff(bac_unique, c("CK", "Syn"))) # 将 CK 和 WAZ 排在最前
drew_data$Bac <- factor(drew_data$Bac, levels = bac_levels)
drew_data$line <- factor(drew_data$line, levels = c("YanAn", "ChangZhi"))

# 自定义颜色 (YanAn 和 ChangZhi)
my_colors <- c("YanAn" = "#E64B35", "ChangZhi" = "#4DBBD5")

line <-  c("YanAn", "ChangZhi")

for (j in 1:length(line)) {
  keyword <- line[j]
  
  
  drew_data_1 <- drew_data[drew_data$line == keyword,]
  
  
  
  
  # 1. 加载库
  library(tidyverse)
  
  # 确保数据存在
  data <- drew_data_1
  

  target_vars <- c("RL", "PH")
  
  # 严格定义 X 轴顺序
  bac_order <- c(
    "Syn", "Pseudomonas", "Sphingomonas", "Variovorax", "Pedobacter", 
    "Sphingobacterium", "Microbacterium", "Caulobacter", "Nocardioides", 
    "Bacillus", "Streptomyces",  "Leifsonia", 
    "Novosphingobium", "Promicromonospora", "Paenarthrobacter"
  )
  
  # 2. 循环处理
  for (var in target_vars) {
    
    #  A. 显著性计算 
    bac_groups <- setdiff(unique(data$Bac), "CK")
    sig_results <- data.frame()
    
    for (g in bac_groups) {
      group_val <- data[[var]][data$Bac == g]
      ck_val <- data[[var]][data$Bac == "CK"]
      
      group_val <- group_val[!is.na(group_val)]
      ck_val <- ck_val[!is.na(ck_val)]
      
      p_obj <- tryCatch({
        t.test(group_val, ck_val)$p.value
      }, error = function(e) {
        if (mean(group_val, na.rm=T) == mean(ck_val, na.rm=T)) return(1) else return(0.01)
      })
      
      sig_results <- rbind(sig_results, data.frame(Bac = g, p_val = p_obj))
    }
    
    # 转换为星号并转为因子
    stat_res <- sig_results %>%
      mutate(label = case_when(
        p_val < 0.001 ~ "***",
        p_val < 0.01  ~ "**",
        p_val < 0.05  ~ "*",
        TRUE ~ ""
      )) %>%
      mutate(Bac = factor(Bac, levels = bac_order)) # 确保显著性表的Bac也是因子
    
    #  B. 数据预处理 
    ck_mean <- mean(data[[var]][data$Bac == "CK"], na.rm = TRUE)
    
    # 汇总绘图数据
    df_plot <- data %>%
      filter(Bac != "CK") %>%
      mutate(diff_val = .data[[var]] - ck_mean) %>%
      # 【关键】：在此处强制转换为因子顺序
      mutate(Bac = factor(Bac, levels = bac_order)) 
    
    # 计算均值和标准误
    df_summary <- df_plot %>%
      group_by(Bac) %>%
      summarise(
        mean_diff = mean(diff_val, na.rm = TRUE),
        se_diff = sd(diff_val, na.rm = TRUE) / sqrt(n()),
        .groups = 'drop'
      ) %>%
      # 合并显著性标注
      left_join(stat_res, by = "Bac")
    
    
    #  C. 绘图 
    p <- ggplot(df_summary, aes(x = Bac, y = mean_diff, fill = Bac)) +
      geom_bar(stat = "identity", width = 0.7, color = "black", alpha = 0.8) +
      geom_errorbar(aes(ymin = mean_diff - se_diff, ymax = mean_diff + se_diff), 
                    width = 0.25, color = "black") +
      # 如果确定不需要抖动点，可以把下面这行注释掉
      geom_jitter(data = df_plot, aes(y = diff_val), 
                  width = 0.15, size = 1.2, alpha = 0.4, color = "black") +
      geom_text(aes(label = label, 
                    y = ifelse(mean_diff >= 0, mean_diff + se_diff, mean_diff - se_diff)),
                vjust = ifelse(df_summary$mean_diff >= 0, -0.8, 1.6),
                size = 5, fontface = "bold") +
      geom_hline(yintercept = 0, linetype = "solid", color = "black") +
      
      # 保持 X 轴顺序
      scale_x_discrete(limits = bac_order, drop = FALSE) + 
      
      # 【新增：设置 Y 轴范围与扩展】
      # mult = c(0.1, 0.2) 表示下方预留 10% 空间，上方预留 20% 空间给星号
      scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
      

      coord_cartesian(
        # ylim = c(-2, 3), 
        clip = "off"
      ) + 
      
      labs(title = paste("Trait:", var," ", keyword), 
           y = "Difference relative to CK mean", x = NULL) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 8),
        panel.grid = element_blank(),
        legend.position = "none"
      )
    
    #  D. 导出 
    file_name <- paste0("Ordered_Final_", var)
    ggsave(paste0("./3_up-down-",keyword, "/", file_name, ".png"), p, width = 5, height = 3, dpi = 300)
    ggsave(paste0("./3_up-down-",keyword, "/", file_name, ".pdf"), p, width = 5, height = 3)
    
    message(paste("Saved plot with correct order for:", var))
  }
  
  
  
}



##### Figure 3C####




rm(list = ls())
gc()

# 设置工作目录
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/3_RNA-seq-analysis/7_DESeq2_results")

library(ggplot2)

# 1. 读取数据
data <- read.table("./2_redrew/1_number/degnumber.txt", header = TRUE, sep = "\t")

# 设置因子顺序 (调控方向 up/down 及类型 gene/ncRNA)
data$regulate <- factor(data$regulate, levels = c("up", "down"))
data$type <- factor(data$type, levels = c("gene", "ncRNA"))

# 2. 定义通用绘图函数
# 封装绘图逻辑，避免代码重复
plot_deg <- function(plot_data, plot_title) {
  p <- ggplot(plot_data, aes(x = group, y = number, fill = regulate)) + 
    # 添加柱状图图层
    geom_bar(
      stat = "identity",                             
      position = position_dodge(preserve = "single"),
      width = 0.9,                                   # 柱子宽度设为 0.9
      color = "black",                               
      linewidth = 0.3                                
    ) +
    # 添加数值标签 (隐藏 0 值)
    geom_text(
      aes(label = ifelse(number > 0, number, "")),
      # 注意：这里的 width 建议与 geom_bar 的 width (0.9) 保持一致，以确保数字在柱子正中间
      position = position_dodge(width = 0.9), 
      vjust = -0.4,
      size = 3.5
    ) +
    # 自定义调控颜色 (上调: 红色，下调: 蓝色)
    #  scale_fill_manual(values = c("up" = "#e74c3c", "down" = "#3498db")) +
    scale_fill_manual(values = c("up" = "#FAC08F", "down" = "#69ACDC")) +
    
    # Y轴扩展：底部紧贴0，顶部向上扩展20%的留白，防止数值标签超出画幅
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(
      title = plot_title,
      x = "Experimental Group",
      y = "Count (number)",
      fill = "Regulation"
    ) +
    theme_bw(base_size = 12) +
    theme(
      # 主标题样式
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      
      # 坐标轴标题字体大小
      axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
      
      axis.title.y = element_text(size = 10, face = "bold"),
      
      # X 轴刻度标签不旋转，恢复水平居中对齐
      #   axis.text.x = element_text(size = 9, color = "black"),
      axis.text.x = element_text(size = 9, angle = 15, hjust = 1, vjust = 1, color = "black"),
      axis.text.y = element_text(size = 9, color = "black"),
      
      # 细节调整
      panel.grid.major.x = element_blank(), # 隐藏X轴主要网格线
      panel.grid.minor.x = element_blank(), # 隐藏X轴次要网格线（保留了Y轴次要网格线）
      legend.position = "right"
    )
  
  return(p)
}


# 3. 分别提取数据并绘图
#  (1) 绘制 Gene 图表 
data_gene <- subset(data, type == "gene")
p_gene <- plot_deg(data_gene, "Differentially Expressed Genes")
print(p_gene)

# 保存 Gene 的 PDF 和 PNG
ggsave("./2_redrew/1_number/deg_grouped_bar_gene-2sep.pdf", plot = p_gene, width = 3.5, height = 2.1, units = "in")
ggsave("./2_redrew/1_number/deg_grouped_bar_gene-2sep.png", plot = p_gene, width = 3.5, height = 2.1, dpi = 300, units = "in")

#  (2) 绘制 ncRNA 图表 
data_ncrna <- subset(data, type == "ncRNA")
p_ncrna <- plot_deg(data_ncrna, "Differentially Expressed ncRNAs")
print(p_ncrna)

# 保存 ncRNA 的 PDF 和 PNG
ggsave("./2_redrew/1_number/deg_grouped_bar_ncRNA-2sep.pdf", plot = p_ncrna, width = 3.5, height = 2.3, units = "in")
ggsave("./2_redrew/1_number/deg_grouped_bar_ncRNA-2sep.png", plot = p_ncrna, width = 3.5, height = 2.3, dpi = 300, units = "in")









##### Figure 3D ####


rm(list = ls())
gc()

# 设置工作目录
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/3_RNA-seq-analysis/7_DESeq2_results")



# 加载必要的 R 包
library(dplyr)
library(ggplot2)
library(stringr) # 用于字符串换行折叠
library(tidyr)   # 【新增】：用于数据行列扩展与拆分
# ggnewscale 用于在同一ggplot中设置两个不同的 color scale (冷暖两套色系)
# 如果未安装，请去掉下方的注释进行安装
# install.packages("ggnewscale")
library(ggnewscale)

# === 1. 数据读取与基础设置 ===
output_dir <- "./2_redrew/2_GO/"
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 读取数据
H18_up <- read.csv("./annotation/2_GO-2/1_H18_vs_Syn_Gene_Up_GO_Enrichment_Fixed.csv", header = T)
H20_up <- read.csv("./annotation/2_GO-2/1_H20_vs_Syn_Gene_Up_GO_Enrichment_Fixed.csv", header = T)
Syn_down <- read.csv("./annotation/2_GO-2/1_CK_vs_Syn_Gene_Up_GO_Enrichment_Fixed.csv", header = T)

main_figure_goterms <- c(
  "rRNA N-glycosylase activity", "negative regulation of translation", "abscisic acid binding",
  "response to water", "galactose metabolic process", "vesicle tethering involved in exocytosis",
  "trehalose biosynthetic process", "alternative oxidase activity", "glutathione metabolic process",
  "superoxide-generating NADPH oxidase activity", "lignin metabolic process", "phenylpropanoid metabolic process",
  "trans-cinnamate 4-monooxygenase activity", "phenol-containing compound biosynthetic process",
  "polyamine biosynthetic process", "P-type proton-exporting transporter activity",
  "proton export across plasma membrane", "calcium-activated cation channel activity",
  "exocyst", "defense response to fungus"
)

# === 2. 数据整合与格式化 ===
# 添加分组标识
H18_up$Group <- "H18_up"
H20_up$Group <- "H20_up"
Syn_down$Group <- "Syn_down"

# 合并三个数据框
df_all <- bind_rows(H18_up, H20_up, Syn_down)

# 根据目标 GO terms 进行提取
df_plot <- df_all %>% filter(Description %in% main_figure_goterms)

# 计算显著性 -log10(p.adjust) 用于颜色映射
df_plot$log10_P <- -log10(df_plot$p.adjust)

# === 3. 因子化处理 (控制绘图顺序) ===
# 控制 X 轴三列的分面顺序 (Syn_down 在前)
df_plot$Group <- factor(df_plot$Group, levels = c("Syn_down", "H18_up", "H20_up"))

# 将 Ontology 替换为缩写 BPMFCC
df_plot$Ontology <- gsub("biological_process|biological process", "BP", df_plot$Ontology, ignore.case = TRUE)
df_plot$Ontology <- gsub("molecular_function|molecular function", "MF", df_plot$Ontology, ignore.case = TRUE)
df_plot$Ontology <- gsub("cellular_component|cellular component", "CC", df_plot$Ontology, ignore.case = TRUE)

# 控制 Y 轴右侧的分面顺序 (从上往下：BP, MF, CC)
df_plot$Ontology <- factor(df_plot$Ontology, levels = c("BP", "MF", "CC"))

# 每个 Ontology 内按照 count 排序
term_order_df <- df_plot %>%
  group_by(Ontology, Description) %>%
  summarise(sort_val = max(Count), .groups = 'drop') %>%
  arrange(Ontology, sort_val, Description)

# 应用新的因子顺序
df_plot$Description <- factor(df_plot$Description, levels = term_order_df$Description)

# === 4. 开始 ggplot2 绘图 ===
p <- ggplot(df_plot, aes(x = RichFactor, y = Description)) +
  

geom_point(data = filter(df_plot, Group %in% c("H18_up", "H20_up")),
           aes(size = Count, color = log10_P)) +
  scale_color_gradient(low = "#FBABA2", high = "#99000D", name = "-log10(P)\n(Up/Warm)") +
  
  # 插入新的颜色标度图层
  new_scale_color() +
  
geom_point(data = filter(df_plot, Group == "Syn_down"),
           aes(size = Count, color = log10_P)) +
  scale_color_gradient(low = "#A2CCFB", high = "#08306B", name = "-log10(P)\n(Down/Cool)") +
  
facet_grid(Ontology ~ Group, scales = "free_y", space = "free_y") +

scale_x_continuous(expand = expansion(mult = 0.25)) +
  scale_y_discrete(expand = expansion(mult = 0.1, add = 0.8), 
                   labels = function(x) str_wrap(x, width = 50)) +
  scale_size_continuous(range = c(1, 4), name = "Gene Count") +

theme_bw() +
  theme(
    axis.title.x = element_text(size = 11, color = "black", face = "bold"),
    axis.title.y = element_text(size = 11, color = "black", face = "bold"),
    
    # 旋转 X 轴标签 45 度，并向右对齐 (hjust = 1)
    axis.text.x = element_text(size = 10, color = "black", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10, color = "black", lineheight = 0.8),
    
    strip.text.x = element_text(size = 11, color = "black", face = "bold"),
    strip.text.y = element_text(size = 11, color = "black", face = "bold", angle = -90),
    strip.background = element_rect(fill = "#f2f2f2", color = "grey60"),
    
    # 【修改处】：减小上下分面之间的垂直距离
    # 默认值通常较大，这里设为 0.1 cm 以拉近 BP、MF、CC 之间的距离
    panel.spacing.y = unit(0.1, "cm"),
    # 如果你也想同时控制左右分面间距，可以取消下方注释
    # panel.spacing.x = unit(0.1, "cm"), 
    
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  labs(x = "Rich Factor", y = "GO Terms")

ggsave(filename = paste0(output_dir, "Combined_GO_Enrichment-uud.pdf"), 
       plot = p, width = 8.2, height = 3.5)
ggsave(filename = paste0(output_dir, "Combined_GO_Enrichment-uud.png"), 
       plot = p, width = 8.2, height = 3.5, dpi = 300)

# 在 Rstudio 预览
print(p)




##### Figure 3E ####

# 20260414 syn - CK - H18 - H23 RNA-seq 与 qPCR 联合分析
rm(list = ls())
gc()

# 1. 设置工作目录 (请修改为你自己的实际路径)
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/3_RNA-seq-analysis/8-定量/")

# 加载包
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. 读取数据
data <- read.table("./drew-data.txt", header = TRUE, sep = "\t")

data <- data %>%
  rename(geneID = gene) %>% # 将原列名 gene 改为 geneID
  mutate(
    geneID = as.character(geneID),
    # 根据对应关系把四位ID替换为完整基因名称
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
    # 将原始数据中的 H18 和 H20 替换为新的样本名称
    group = case_when(
      group == "H18" ~ "Syn_Pse.",
      group == "H20" ~ "Syn_Sph.",
      TRUE ~ as.character(group)
    )
  ) %>%
  # 【核心修改】：去除不需要绘制的基因
  filter(geneID != "TraesCS4D02G063100")

# 基于 X2ddct 列重新计算 Mean 和 SD，确保后续绘图数据完全由脚本生成
data <- data %>%
  rowwise() %>%
  mutate(
    # 重新计算均值和样本标准差，na.rm=TRUE 防止个别缺失值报错
    calc_mean = mean(c(X2ddct_1, X2ddct_2, X2ddct_3), na.rm = TRUE),
    calc_SD = sd(c(X2ddct_1, X2ddct_2, X2ddct_3), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # 覆盖原有的 mean 和 SD 列
  mutate(
    mean = calc_mean,
    SD = calc_SD
  )

# -- 数据处理与统计分析 -- #

# 提取 Delta CT (dct) 的原始数据用于严谨的 t-test 显著性计算
data_stats <- data %>%
  select(geneID, group, dct_1, dct_2, dct_3) %>%
  pivot_longer(cols = starts_with("dct"), names_to = "Rep", values_to = "dct_value")

# 批量计算以 Syn 为对照的显著性 p 值，并转换为星号
sig_df <- data_stats %>%
  group_by(geneID) %>%
  summarise(
    # 提取 Syn 组的 dct 值作为 baseline
    syn_vals = list(dct_value[group == "Syn"]),
    # 分别计算另外三组的 p 值 
    p_CK = t.test(dct_value[group == "CK"], syn_vals[[1]])$p.value,
    p_Syn_Pse. = t.test(dct_value[group == "Syn_Pse."], syn_vals[[1]])$p.value,
    p_Syn_Sph. = t.test(dct_value[group == "Syn_Sph."], syn_vals[[1]])$p.value,
    .groups = 'drop'
  ) %>%
  # 宽数据转长数据，方便后续合并 (使用 ^p_ 精确匹配替换前缀)
  pivot_longer(cols = starts_with("p_"), names_to = "group", values_to = "p_val") %>%
  mutate(
    group = gsub("^p_", "", group),
    # 根据 p 值赋予显著性星号
    sig = case_when(
      p_val < 0.001 ~ "***",
      p_val < 0.01  ~ "**",
      p_val < 0.05  ~ "*",
      TRUE          ~ ""
    )
  )

# 为对照组 (Syn) 添加一行空标记，用于合并
syn_df <- data.frame(geneID = unique(data$geneID), group = "Syn", p_val = NA, sig = "")
sig_df <- bind_rows(sig_df, syn_df)

# 【核心优化】：定义基因分面的目标顺序（已去除 TraesCS4D02G063100）
gene_order <- c("TraesCS2A02G026600", "TraesCS4A02G229100", "TraesCS7A02G258800",
                "TraesCS2D02G317800", "TraesCS2B02G109700",
                "TraesCS5B02G488700")

# 将显著性星号合并回用于绘图的主数据框中
plot_data <- data %>%
  left_join(sig_df, by = c("geneID", "group")) %>%
  mutate(
    # 设置 x 轴分组顺序为指定的新名称，绘图顺序保持不变
    group = factor(group, levels = c("Syn", "CK", "Syn_Pse.", "Syn_Sph.")),
    # 将 geneID 设为 factor，并使用自定义的 levels 控制 facet_wrap 顺序
    geneID = factor(geneID, levels = gene_order)
  )


# -- 绘图步骤 -- #

# 设置柱子颜色的 map
treatment_colors_map <- c(
  "CK"       = "#00A087",  # 科研青绿 (Control首选)
  "Syn"      = "#4DBBD5",  # 亮蓝
  "Syn_Pse." = "#FB6E1C",  # 正红
  "Syn_Sph." = "#F39B7F"   # 橘粉
)

p <- ggplot(plot_data, aes(x = group, y = mean, fill = group)) +
  
  # 绘制柱状图
  geom_col(color = "black", width = 0.65, linewidth = 0.6, alpha = 0.8) +
  
  # 添加误差棒，这里的 mean 和 SD 已经是我们重新计算过的新列
  geom_errorbar(aes(ymin = mean, ymax = mean + SD), width = 0.2, linewidth = 0.7) +
  
  # 直接使用 geom_text 将刚刚计算好的星号打在误差棒顶端
  geom_text(aes(y = mean + SD, label = sig), 
            vjust = -0.0,     # 向上偏移，避免与误差棒重叠
            size = 4,         # 星号字体大小
            color = "black", 
            fontface = "bold") +
  
  # 分面图，调整为 ncol = 3，正好放下6个基因形成 2x3 布局
  # facet_wrap(~ geneID, nrow = 2, ncol = 3, scales = "free") +
  facet_wrap(~ geneID, nrow = 1, ncol = 6, scales = "free") +
  # 动态扩展 Y 轴上限，防止最高的星号被图形边缘切割
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  
  # 图形主题调整
  theme_classic() +
  
  # 应用自定义的颜色地图
  scale_fill_manual(values = treatment_colors_map) +
  
  labs(x = "Treatment Groups", 
       y = expression("Relative Expression (2"^-paste(Delta, Delta, "C"["T"])*")")) +
  
  theme(
    axis.title.x = element_text(size = 10, color = "black", face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 10, color = "black", face = "bold", margin = margin(r = 10)),
    # X轴标签旋转45度，并设置 hjust = 1 保证对齐
    axis.text.x = element_text(size = 9, color = "black", angle = 25, hjust = 1),
    axis.text.y = element_text(size = 9, color = "black"),
    strip.text = element_text(size = 7, face = "bold", color = "black"), # 分面标题根据长基因名称适当调小字号避免拥挤
    strip.background = element_blank(),
    legend.position = "none" 
  )

# 打印图像
print(p)


# 保存高分辨率图片（文件命名同步更新为6genes）
ggsave("qPCR_Expression_6genes_Corrected-63100-2.pdf", plot = p, width = 5, height = 3.0)
ggsave("qPCR_Expression_6genes_Corrected-63100-2.png", plot = p, width = 5, height = 3.0, dpi = 300)

# 保存高分辨率图片（文件命名同步更新为6genes）
ggsave("qPCR_Expression_6genes_Corrected-63100-3.pdf", plot = p, width = 7, height = 2.0)
ggsave("qPCR_Expression_6genes_Corrected-63100-3.png", plot = p, width = 7, height = 2.0, dpi = 300)




####******************************************************####


#### Figure 4 ####


##### Figure 4A ####

rm(list = ls())
gc()


# 1. 加载必要的 R 包

library(phyloseq)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(reshape2)

setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/4_metadata_analysis/")

group <- c("Bracken",  "Kaiju_fungi")
class <- c("genus")

for (i in 1:length(group)){
  
  #  i <- 1
  method <- group[i]
  
  output_dir <- paste0("./2_analysis_R/1_species/3_Abundance-2/", method)
  # recursive=TRUE 允许创建多级目录
  if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  for (j in 1:length(class)) {
    #    j <- 1
    keyword <- class[j]
    
    output_dir2 <-  paste0(output_dir,"/",keyword )
    if(!dir.exists(output_dir2)) dir.create(output_dir2, recursive = TRUE)
    
    group_col <- "Group"      # 设定分组列名
    control_group <- "Syn"    # 设定对照组名称
    

    # 2. 读取过滤后的 phyloseq 对象

    if (method == "Kaiju_fungi" ) {
      ps_clean <- readRDS(paste0("./2_analysis_R/1_species/0_data/2_Kaiju_", keyword, "_cleaned_fungi.rds"))
    } else {
      ps_clean <- readRDS(paste0("./2_analysis_R/1_species/0_data/2_",method,"_", keyword, "_cleaned.rds"))
    }
    

    # 3. 丰度转换与数据长格式化

    # 将绝对丰度转化为相对丰度 (乘以 100 转换为百分比)
    ps_rel <- transform_sample_counts(ps_clean, function(x) x / sum(x) * 100)   
    ps_melt <- psmelt(ps_rel)   # 转化为长格式，方便 ggplot 绘图和 dplyr 操作
    
    =======
    # 【核心修复】：解决 "找不到对象'OTU'" 报错
    # 错误原因分析：phyloseq 的 psmelt 函数会将 feature ID（即 OTU/Taxon 名）
    # 所在的列默认命名为 "OTU"。但在较新版本的 phyloseq 或特定数据导入方式中，
    # 这一列可能被重命名为其他名字（例如 "Taxon" 或 "FeatureID"）。
    # 修复逻辑：在进行后续操作前，强制检查并将代表物种名的第一列重命名为 "OTU"。
    =======
    
    # 检查是否存在 "OTU" 列，如果不存在，尝试将通常代表物种名的 "Taxon" 重命名为 "OTU"
    if (!"OTU" %in% colnames(ps_melt)) {
      if ("Taxon" %in% colnames(ps_melt)) {
        ps_melt <- ps_melt %>% rename(OTU = Taxon)
      } else {
        # 如果连 Taxon 列也没有，输出可用列名以供排查，并停止运行
        stop(paste("Error: 'OTU' column not found in ps_melt. Available columns are:", paste(colnames(ps_melt), collapse = ", ")))
      }
    }
    
    
    # 4. 筛选丰度 > 1% 的优势属
    
    # 计算每个物种在每个组内的平均相对丰度
    df_summary <- ps_melt %>%
      group_by(!!sym(group_col), OTU) %>%
      summarise(Mean_Abun = mean(Abundance), .groups = "drop")
    
    # 找出在任意一个组中平均相对丰度 > 1% 的物种
    top_taxa <- df_summary %>%
      group_by(OTU) %>%
      summarise(Max_Mean = max(Mean_Abun)) %>%
      filter(Max_Mean > 1) %>%
      pull(OTU)
    
    cat("共提取出", length(top_taxa), "个相对丰度 > 1% 的优势属进行差异分析。\n")
    
    # 如果 top_taxa 为空，说明没有优势属，跳过当前循环以防后续计算报错
    if (length(top_taxa) == 0) {
      cat("警告: 没有丰度大于 1% 的优势类群，跳过绘图。\n")
      next 
    }
    
    
    # 5. 差异显著性检验 (T-test, 评估上下调)
    
    df_top <- ps_melt %>% filter(OTU %in% top_taxa)
    
    other_groups <- setdiff(unique(df_top[[group_col]]), control_group)
    stat_list <- list()
    
    # 循环计算每个属中，其他组与 Syn 组的差异
    for(tax in top_taxa) {
      tax_data <- df_top %>% filter(OTU == tax)
      syn_abun <- tax_data %>% filter(!!sym(group_col) == control_group) %>% pull(Abundance)
      
      for(grp in other_groups) {
        grp_abun <- tax_data %>% filter(!!sym(group_col) == grp) %>% pull(Abundance)
        
        # 确保对照组和比较组都有足够的样本(>=3)来进行统计检验
        if(length(syn_abun) >= 3 & length(grp_abun) >= 3) {
          # 使用参数检验 Student's t-test
          test <- t.test(grp_abun, syn_abun)
          
          # 判断上下调关系: 比较组均值 > 对照组均值 -> 上调(Up)；反之 -> 下调(Down)
          reg_status <- ifelse(mean(grp_abun) > mean(syn_abun), "Up", "Down")
          
          stat_list[[paste(tax, grp, sep="_")]] <- data.frame(
            Taxon = tax,
            Compare_Group = grp,
            Control_Group = control_group,
            p_value = test$p.value,
            y_max = max(c(syn_abun, grp_abun)), # 记录最大值，用于图中定位星号
            Regulation = reg_status              # 记录上下调状态
          )
        }
      }
    }
    
    stat_df <- do.call(rbind, stat_list)

    stat_df$signif <- ifelse(stat_df$p_value < 0.001, "***",
                             ifelse(stat_df$p_value < 0.01, "**",
                                    ifelse(stat_df$p_value < 0.05, "*", "")))
    
    # 保存统计结果到 CSV
    write.csv(stat_df, paste0(output_dir2,"/Stats_Difference_vs_Syn_", keyword, ".csv"), row.names = FALSE)
    cat("差异分析统计表（含上下调信息，未进行 p 值校正）已导出。\n")
    
    # 过滤出显著的结果，用于画图标记
    stat_signif <- stat_df %>% filter(signif != "")
    
    
    # 6. 绘图 A：整体相对丰度堆叠图
    
    group_order <- c("Syn","CK",  "H18", "H20") # X 轴显示顺序
    color_style <- "Paired"                     # 颜色方案备选: "Set1", "Set3", "Dark2"
    
    df_plot <- df_summary %>%
      filter(OTU %in% top_taxa) %>% 
      rename(Taxon = OTU) %>% 
      group_by(!!sym(group_col), Taxon) %>%
      summarise(Abundance = sum(Mean_Abun), .groups = "drop")
    
    # 锁定 X 轴分组顺序
    df_plot[[group_col]] <- factor(df_plot[[group_col]], levels = group_order)
    
    # 按整体丰度由大到小排序，确保在图例和柱子中一致的视觉呈现
    taxon_rank <- df_plot %>%
      group_by(Taxon) %>%
      summarise(Total_Abundance = sum(Abundance)) %>%
      arrange(desc(Total_Abundance)) 
    
    taxa_levels <- taxon_rank$Taxon
    df_plot$Taxon <- factor(df_plot$Taxon, levels = taxa_levels)
    
    # 计算 Y 轴动态最大值以适应图表比例
    group_sums <- df_plot %>%
      group_by(!!sym(group_col)) %>%
      summarise(TotalAbundance = sum(Abundance), .groups = "drop")
    y_max <- max(group_sums$TotalAbundance)
    y_limit <- y_max * 1.05 
    
    # 动态分配分类层级颜色
    num_colors <- length(taxa_levels)
    my_colors <- colorRampPalette(brewer.pal(min(num_colors, 12), color_style))(num_colors)
    names(my_colors) <- taxa_levels
    
    p_bar <- ggplot(df_plot, aes(x = !!sym(group_col), y = Abundance, fill = Taxon)) +
      geom_bar(stat = "identity", position = "stack", color = "grey30", width = 0.7, linewidth = 0.15) +
      scale_fill_manual(values = alpha(my_colors, 0.9)) +
      labs(
        x = "Experimental Groups", 
        y = "Relative Abundance (>1% Taxa, %)", 
        fill = keyword
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black", face = "bold"),
        axis.text.y = element_text(size = 11, color = "black"),
        axis.title = element_text(size = 13, face = "bold"),
        legend.text = element_text(size = 8, face = "italic"), 
        legend.title = element_text(size = 11, face = "bold"),  
        legend.key.size = unit(0.41, "cm"), 
        panel.grid = element_blank(),
        panel.border = element_rect(linewidth = 0.8)
      ) +
      guides(fill = guide_legend(ncol = 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, y_limit))
    
    file_base <- paste0(output_dir2,"/Taxa_Barplot_Sorted_", keyword)
    ggsave(paste0(file_base, ".pdf"), p_bar, width = 3.5, height = 5)
    ggsave(paste0(file_base, ".png"), p_bar, width = 3.5, height = 5, dpi = 300)
    
    cat("堆叠图表已导出。\n")
    
    
   
    # 8. 绘图 C：T-test 显著性符号网格图
    
    grid_groups <- c("CK", "H18", "H20")  # H18--Pse, H20--Sph
    grid_data <- expand.grid(Taxon = taxa_levels, Compare_Group = grid_groups)
    
    # 提取需要的统计列进行映射
    signif_extract <- stat_df[, c("Taxon", "Compare_Group", "signif", "Regulation")]
    plot_grid_df <- merge(grid_data, signif_extract, by = c("Taxon", "Compare_Group"), all.x = TRUE)
    
    # 处理空白或非显著 NA 数据
    plot_grid_df$signif[is.na(plot_grid_df$signif)] <- ""
    plot_grid_df$Regulation[is.na(plot_grid_df$Regulation)] <- "None"
    
    plot_grid_df$Taxon <- factor(plot_grid_df$Taxon, levels = rev(taxa_levels))
    plot_grid_df$Compare_Group <- factor(plot_grid_df$Compare_Group, levels = grid_groups)
    
    p_grid <- ggplot(plot_grid_df, aes(x = Compare_Group, y = Taxon)) +
      geom_tile(fill = "white", color = "grey70", linewidth = 0.5) + 
      geom_text(aes(label = signif, color = Regulation), size = 6, fontface = "bold", vjust = 0.75) + 
      scale_color_manual(values = c("Up" = "red", "Down" = "blue", "None" = "black")) +
      scale_x_discrete(position = "top") + 
      labs(x = "Comparison vs Syn", y = "") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 11, face = "italic", color = "black"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none" 
      )
    
    ggsave(paste0(output_dir2, "/PlotC_Signif_Grid_", keyword, ".pdf"), p_grid, width = 3.5, height = 5)
    ggsave(paste0(output_dir2, "/PlotC_Signif_Grid_", keyword, ".png"), p_grid, width = 3.5, height = 5, dpi = 300)
    
    cat("T-test 显著性标注网格图已导出。\n\n")
    
   
    
  } # 结束 class 循环
} # 结束 group 循环





##### Figure 4B ####



rm(list = ls()) 
gc()             


# 第一阶段：环境设置与依赖包加载

# 设置你的工作目录
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/4_metadata_analysis/")

# 建立层级化输出目录
outDir_DEG         <- "./2_analysis_R/2_Gene-path/3_DEseq2_gene"            

outDir_VennUpSet   <- file.path(outDir_DEG, "Venn_UpSet_Plots") # 【新增】专门存放韦恩图和UpSet的目录

if(!dir.exists(outDir_DEG))         dir.create(outDir_DEG, recursive = TRUE)

if(!dir.exists(outDir_VennUpSet))   dir.create(outDir_VennUpSet, recursive = TRUE)

# 加载依赖包 
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(RColorBrewer)
library(reshape2)
library(DESeq2)
library(ggVennDiagram)
library(data.table)
library(clusterProfiler)
library(enrichplot)
library(GO.db)
library(KEGGREST)
library(UpSetR)
# # 1. 安装并加载包
# install.packages("ggrastr")
library(ggrastr)





# 第二阶段：数据读取与 DESeq2 建模准备

cat("📊 正在读取并清洗 Counts 和 Taxonomy 数据...\n")

count_data <- read.table("./2_analysis_R/2_Gene-path/0_data_clean/1_Clean_bac_count.tsv", 
                         header=TRUE, row.names=1, sep="\t", check.names = FALSE)
count_data <- round(count_data) 

# 构建样本信息表 (Metadata)
sample_names <- colnames(count_data)
groups <- gsub("_.*", "", sample_names)
metadata <- data.frame(
  SampleID = sample_names,
  Group    = factor(groups, levels = c("Syn","CK",  "H18", "H20")) # H18--Pse； H20Sph
)
rownames(metadata) <- metadata$SampleID 

cat("✂️ 正在执行表达量过滤...\n")
keep <- rowSums(count_data >= 10) >= 3 
count_data_filter <- count_data[keep, ]
bg_universe <- rownames(count_data_filter)

cat("⚙️ 正在运行 DESeq2 模型...\n")
dds <- DESeqDataSetFromMatrix(countData = count_data_filter, colData = metadata, design = ~ Group)
dds <- DESeq(dds) 

padj_cutoff <- 0.05
log2fc_cutoff <- 1.0  

comparisons <- list(
  c("Group", "CK", "Syn" ), 
  c("Group", "H18", "Syn"),
  c("Group", "H20", "Syn")
  #  c("Group", "Syn", "CK")
)


if (!requireNamespace("ggrastr", quietly = TRUE)) {
  cat("⚠️ 未检测到 ggrastr 包，正在为您自动安装...\n")
  install.packages("ggrastr")
}
library(ggrastr)


# 第三阶段：核心差异分析与火山图 (数据导出)

for(comp in comparisons){
  
  # comp <- comparisons[1]
  
  trt_name <- comp[2]
  ctrl_name <- comp[3]
  comp_name <- paste0(trt_name, "_vs_", ctrl_name)
  cat("\n>>> ======= <<<\n")
  cat(">>> 正在处理差异比较组:", comp_name, "\n")
  
  # 1. 差异提取与清洗
  res <- results(dds, contrast = comp)
  res_df_all <- as.data.frame(na.omit(res)) 
  

  res_df_all$GeneID <- rownames(res_df_all)
  res_df_all <- res_df_all[, c("GeneID", setdiff(names(res_df_all), "GeneID"))]
  
  # 2. 标记显著性
  res_df_all$Sig <- "Not-Sig"
  res_df_all$Sig[res_df_all$padj < padj_cutoff & res_df_all$log2FoldChange > log2fc_cutoff] <- "Up"
  res_df_all$Sig[res_df_all$padj < padj_cutoff & res_df_all$log2FoldChange < -log2fc_cutoff] <- "Down"
  res_df_all$Sig <- factor(res_df_all$Sig, levels = c("Up", "Down", "Not-Sig"))
  
  up_num <- sum(res_df_all$Sig == "Up")
  down_num <- sum(res_df_all$Sig == "Down")
  cat("  -> 发现上调:", up_num, "| 下调:", down_num, "\n")
  
  # 3. 数据落盘 (完全解耦的核心)
  write.csv(res_df_all, file = file.path(outDir_DEG, paste0("DESeq2_All_", comp_name, ".csv")), row.names = FALSE)
  write.csv(res_df_all[res_df_all$Sig != "Not-Sig", ], file = file.path(outDir_DEG, paste0("DESeq2_SigOnly_", comp_name, ".csv")), row.names = FALSE)
  
  # # 4. 火山图绘制
  
  # 1. 核心对称边界计算（加入 5% 的留白缓冲，防止边缘处的点被切掉）
  xlim_max <- max(abs(res_df_all$log2FoldChange), na.rm = TRUE) * 1.05
  ylim_max <- max(-log10(res_df_all$padj[res_df_all$padj > 0]), na.rm = TRUE) * 1.05
  
  p_volcano <- ggplot(res_df_all, aes(x = log2FoldChange, y = -log10(padj), color = Sig)) +
    
    # 【★已修复★】：使用 ggrastr 推荐的 rasterise 包装器，完美栅格化超多散点
    rasterise(geom_point(alpha = 0.6, size = 1.0), dpi = 300, dev = "ragg") + 
    
    scale_color_manual(values = c("Up" = "#E64B35", "Down" = "#4DBBD5", "Not-Sig" = "grey80")) + 
    
    # 强制限制 X 轴范围为 [-xlim_max, xlim_max]，实现绝对的左右对称
    scale_x_continuous(limits = c(-xlim_max, xlim_max)) +
    scale_y_continuous(limits = c(0, ylim_max), expand = expansion(mult = c(0, 0.02))) + # Y轴底端对齐0，顶端微留白
    
    # 辅助线
    geom_vline(xintercept = c(-log2fc_cutoff, log2fc_cutoff), linetype = "dashed", color = "black", linewidth = 0.5) + 
    geom_hline(yintercept = -log10(padj_cutoff), linetype = "dashed", color = "black", linewidth = 0.5) +             
    
    theme_bw() + 
    labs(title = paste0("Volcano Plot: ", comp_name), x = "log2(Fold Change)", y = "-log10(Adjusted P-value)") +
    
    # 优化数量标签的 X 轴坐标，直接采用 xlim_max 的比例定位，保证无论数据怎么变，文字位置永远对称美观
    annotate("text", x = xlim_max * 0.95, y = ylim_max * 0.95, 
             label = paste0("Up: ", up_num), color = "#E64B35", fontface = "bold", hjust = 1, size = 3.5) +
    annotate("text", x = -xlim_max * 0.95, y = ylim_max * 0.95, 
             label = paste0("Down: ", down_num), color = "#4DBBD5", fontface = "bold", hjust = 0, size = 3.5) +
    
    # 字体与排版精细化设置
    theme(
      panel.grid = element_blank(), 
      legend.position = "right", 
      legend.title = element_blank(), 
      
      # 主标题
      plot.title = element_text(hjust = 0.5, face = "bold", size = 11, margin = margin(b = 10)),
      
      # X 轴和 Y 轴标签字体大小、加粗及边距
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)), 
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)), 
      
      # 坐标轴刻度文字大小与颜色
      axis.text = element_text(size = 12, color = "black"),
      
      # 精细控制图例的大小，防止在紧凑画布下显得突兀
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 10)
    )
  
  # 导出优化：由于您导出的尺寸 (3.8 x 2.8) 较紧凑，混合导出能让生成的 PDF 只有几十 KB 且绝不卡顿
  ggsave(filename = file.path(outDir_DEG, paste0("Volcano_", comp_name, ".pdf")), plot = p_volcano, width = 3.8, height = 2.8)
  ggsave(filename = file.path(outDir_DEG, paste0("Volcano_", comp_name, ".png")), plot = p_volcano, width = 3.8, height = 2.8, dpi = 300)
  
}
# === 差异分析大循环在此结束 ===

cat("\n✅ DESeq2 核心分析结束，所有高清混合矢量图及结果已成功存入硬盘！\n")  




##### Figure 4C ####



rm(list = ls()) 

gc()            


if(!require(ggplot2)) install.packages("ggplot2")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(dplyr)) install.packages("dplyr")
# KEGGREST: 提供KEGG数据库的REST API接口，用于获取功能注释信息
if(!require(KEGGREST)) BiocManager::install("KEGGREST")
library(ggplot2)
library(ggrepel)
library(dplyr)
library(KEGGREST)
library(ggrastr)

# 1. 设置全局工作目录与输出定位

setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/4_metadata_analysis/")
funcunt_Dir <- "./2_analysis_R/2_Gene-path/12_Group_vocano"
if(!dir.exists(funcunt_Dir)) dir.create(funcunt_Dir, recursive = TRUE)

databases <- c("KEGG", "CAZy", "antiSMASH", "NCycDB", "PCycDB", "SCycDB", "FeGenie")
DE_KO_dir <- "./2_analysis_R/2_Gene-path/4_EGGnog/5_KEGGPathway_Module_KO-DE/KEGG_ko/"
DE_CAZY_dir <- "./2_analysis_R/2_Gene-path/5_CAZy/2_DEG/"
DE_antiSMASH_dir <- "./2_analysis_R/2_Gene-path/6_antiSMASH/2_BGC_Differential_Analysis/tables/"
DE_N_dir <- "./2_analysis_R/2_Gene-path/7_NCycDB-PCycDB-SCycDB/1_annotation/ncycdb/"
DE_P_dir <- "./2_analysis_R/2_Gene-path/7_NCycDB-PCycDB-SCycDB/1_annotation/pcycdb/"
DE_S_dir <- "./2_analysis_R/2_Gene-path/7_NCycDB-PCycDB-SCycDB/1_annotation/scycdb/"
DE_Fe_dir <- "./2_analysis_R/2_Gene-path/8_FeGenie/2_DEG/"
FC_cutoff <- 1.0
P_cutoff <- 0.05
TOP_N_LABELS <- 3



# 2. 【功能字典获取与解析】(🎯 优化：智能截取短基因名)


cat("  - 正在通过 KEGGREST 获取官方最新描述字典...\n")
kegg_dict <- c()
tryCatch({
  # 从KEGG数据库获取所有KO（KEGG Orthology）条目的描述信息
  tmp <- keggList("ko")
  # 清理ID格式：统一将"ko:"前缀去除并以大写K开头（如K12345）
  names(tmp) <- gsub("^(ko:)?(K|k)", "K", names(tmp)) 
  # 将获取的字典赋值给全局变量
  kegg_dict <- tmp
  # 提示下载成功
  cat("    ✅ 官方字典下载并清洗成功！\n")
}, error = function(e) {
  # 如果下载失败（如网络问题），发出警告并使用原始ID
  cat("    ⚠️ 警告：获取 KEGG 官方在线字典失败，将使用原始 ID 代替功能描述。\n")
})


get_feature_label <- function(ids) {
  # 如果KEGG字典为空，直接返回原始ID
  if (length(kegg_dict) == 0) return(ids)
  # 初始化结果向量
  res <- character(length(ids))
  # 遍历每个ID
  for (j in seq_along(ids)) {
    # 如果ID为"Others"，保留原样
    if (ids[j] == "Others") {
      res[j] <- "Others"
    } else {
      # 移除可能的"ko:"前缀，获得干净的KO编号
      clean_id <- gsub("^ko:", "", ids[j]) 
      # 从字典中查找对应的功能描述
      desc <- kegg_dict[clean_id]
      # 如果找不到描述，标记为"Unknown"
      if (is.na(desc) || length(desc) == 0) {
        desc <- "Unknown"
      } else {
        # 清理描述文本：移除" - "及其后的内容
        desc <- gsub(" - .*", "", desc)
        # 移除方括号内的BR参考信息
        desc <- gsub(" \\[BR:.*\\]", "", desc)
        
        # 🎯 核心优化：如果描述中包含分号，只提取分号前的基因简称
        if (grepl(";", desc)) {
          desc <- trimws(strsplit(desc, ";")[[1]][1])
        }
        # 如果描述仍然超过20个字符，截断并添加省略号
        if (nchar(desc) > 20) desc <- paste0(substr(desc, 1, 17), "...")
      }
      # 组合最终标签：ID + 冒号 + 功能描述
      res[j] <- paste0(clean_id, ": ", desc)
    }
  }
  # 返回结果向量
  return(res)
}






# 3. 【循环执行比较组：读取、聚合整理与联合绘图】

comparisons <- c("H18_vs_Syn", "H20_vs_Syn", "CK_vs_Syn")  # H18--Pse；H20--Sph

# 定义7个数据库对应的颜色（用于横条底色）
# 使用柔和的配色方案，每个数据库有独特的识别色
banner_colors <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69")
names(banner_colors) <- databases

# 开始遍历每个比较组
for (Keyword in comparisons) {
  # 输出当前正在处理的比较组名称
  cat(sprintf("\n>>> 正在处理比较组: %s <<<\n", Keyword))
  
  # 初始化空的数据框，用于存储当前比较组所有数据库的合并数据
  combined_df <- data.frame()
  
  # 遍历每个功能数据库
  for (db in databases) {
    # 根据数据库名称使用switch语句构建对应的文件路径
    file_path <- switch(db,
                        "KEGG"      = paste0(DE_KO_dir, "KEGG_ko_", Keyword, "_All.txt"),
                        "CAZy"      = paste0(DE_CAZY_dir, "1_DEG_AllResults_", Keyword, ".tsv"),
                        "antiSMASH" = paste0(DE_antiSMASH_dir, "DESeq2_All_Type_", Keyword, ".tsv"),
                        "NCycDB"    = paste0(DE_N_dir, Keyword, "/", Keyword, "_All_Results.tsv"),
                        "PCycDB"    = paste0(DE_P_dir, Keyword, "/", Keyword, "_All_Results.tsv"),
                        "SCycDB"    = paste0(DE_S_dir, Keyword, "/", Keyword, "_All_Results.tsv"),
                        "FeGenie"   = paste0(DE_Fe_dir, "1_DEG_AllResults_", Keyword, ".tsv")
    )
    
    # 检查文件是否存在且大小大于0（非空文件）
    if (file.exists(file_path) && file.info(file_path)$size > 0) {
      # 读取制表符分隔的数据文件
      df <- read.table(file_path, header = TRUE, sep = "\t")
      
      # 如果数据框为空，跳过该数据库
      if (nrow(df) == 0) {
        cat(sprintf("   - [警告] %s 的数据文件内容为空，将跳过该组。\n", db))
        next
      }
      
      # 根据数据库类型进行数据格式标准化
      # 统一列名：ID（特征标识）、Sig（显著性方向）
      if(db == "KEGG") {
        # KEGG: Feature列作为ID，Significance列作为显著性
        df <- df %>% rename(ID = Feature, Sig = Significance) %>% mutate(Label = get_feature_label(ID))
      } else if(db == "CAZy") {
        # CAZy: CAZy_Family列作为ID
        df <- df %>% rename(ID = CAZy_Family) %>% mutate(Label = ID)
      } else if(db == "antiSMASH") {
        # antiSMASH: FeatureID作为ID，Change列作为显著性
        df <- df %>% rename(ID = FeatureID, Sig = Change) %>% mutate(Label = ID)
      } else if(db %in% c("NCycDB", "PCycDB", "SCycDB")) {
        # NCycDB/PCycDB/SCycDB: Func_ID作为ID
        df <- df %>% rename(ID = Func_ID) %>% mutate(Label = ID)
      } else if(db == "FeGenie") {
        # FeGenie: HMM作为ID，如果ID过长则截断
        df <- df %>% rename(ID = HMM) %>% mutate(Label = ifelse(nchar(ID) > 25, paste0(substr(ID, 1, 22), "..."), ID))
      }
      
      # 进一步标准化数据：添加数据库来源列，统一显著性标签格式
      df <- df %>% 
        mutate(
          Database = db,  # 添加数据库名称列
          # 将各种显著性表述统一为"Up"、"Down"、"NotSig"三种
          Sig = case_when(
            Sig %in% c("Up", "Up-regulated") ~ "Up",
            Sig %in% c("Down", "Down-regulated") ~ "Down",
            TRUE ~ "NotSig"
          ),
          # 处理缺失或零值的padj：将其设为极小值1e-300避免计算问题
          padj = ifelse(is.na(padj) | padj == 0, 1e-300, padj) 
        ) %>%
        # 选择需要的列，保持数据整洁
        select(ID, log2FoldChange, padj, Sig, Label, Database)
      
      # 将当前数据库的数据合并到总数据框中
      combined_df <- bind_rows(combined_df, df)
    } else {
      # 如果文件不存在或为空，输出警告信息
      cat(sprintf("   - [警告] 找不到 %s 的数据文件或文件为空，将跳过该组。\n", db))
    }
  }
  
  # 检查合并后的数据框是否包含数据
  if (nrow(combined_df) > 0) {
    # 将Database列转换为因子，确保按预定义的数据库顺序显示
    combined_df$Database <- factor(combined_df$Database, levels = databases)
    
    # 生成随机抖动坐标，避免散点完全重叠
    # 设置随机种子确保结果可重复
    set.seed(42)
    combined_df <- combined_df %>%
      mutate(
        base_x = as.numeric(Database),  # 计算每个数据点在x轴上的基础位置
        jitter_x = base_x + runif(n(), min = -0.35, max = 0.35)  # 添加随机偏移
      )
    
    # 筛选需要标注引线标签的数据点
    # 条件：满足显著上调或下调（Sig），且通过过滤卡值阈值
    top_labels <- combined_df %>% 
      filter(Sig %in% c("Up", "Down")) %>%
      filter(abs(log2FoldChange) >= FC_cutoff & padj < P_cutoff) %>%
      # 按数据库和差异方向（Up/Down）分别进行分组
      group_by(Database, Sig) %>%
      # 按照 log2FoldChange 的绝对值从大到小进行降序排列（优先筛选FC最大的基因）
      arrange(desc(abs(log2FoldChange))) %>%
      # 每个功能数据库的每个方向严格截取前 4 个
      slice_head(n = 4) %>% 
      ungroup()
    
    # 拆分上调和下调标签数据，便于分别控制其显示位置
    top_labels_up <- top_labels %>% filter(Sig == "Up")
    top_labels_down <- top_labels %>% filter(Sig == "Down")
    
    # 计算y轴范围的空间参数
    # 获取所有log2FoldChange绝对值的最大值
    y_max_absolute <- max(abs(combined_df$log2FoldChange), na.rm = TRUE)
    
    # y轴上限设为最大值的1.2倍，为顶部和底部分散标签留出更充裕的空间
    y_limit <- y_max_absolute * 1.2  
    # 横条高度设为y轴范围的5.5% (相对高度，此处取0.08)
    banner_height <- y_limit * 0.08  
    
    # 创建背景条纹数据框，用于交替颜色区分不同数据库
    bg_df <- data.frame(
      xmin = seq(0.5, length(databases) - 0.5, by = 1),  # 每个数据库区域的左边界
      xmax = seq(1.5, length(databases) + 0.5, by = 1),  # 每个数据库区域的右边界
      ymin = -Inf,  # y轴下边界无限延伸
      ymax = Inf,   # y轴上边界无限延伸
      fill_color = rep(c("grey94", "white"), length.out = length(databases))  # 交替使用浅灰和白色
    )
    
    # 创建横条标签数据框（显示数据库名称）
    banner_df <- data.frame(
      Database = factor(databases, levels = databases),
      x = 1:length(databases),  # x轴位置
      label = databases  # 显示的文字
    )
    
    # 创建ggplot对象 - 开始构建火山图
    p <- ggplot() +
      # 底层条纹：为每个数据库区域添加交替背景色
      geom_rect(data = bg_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                fill = bg_df$fill_color, alpha = 0.8, inherit.aes = FALSE) +
      
      # 辅助虚线：在FC阈值位置添加虚线，方便识别显著差异边界
      geom_hline(yintercept = c(-FC_cutoff, FC_cutoff), linetype = "dashed", color = "grey50", linewidth = 0.3) +
      
      # 散点图层：【★已修复★】：使用 ggrastr 推荐的 rasterise 包装器，完美栅格化超多散点
      rasterise(
        geom_point(data = combined_df, 
                   aes(x = jitter_x, y = log2FoldChange, color = Sig), 
                   alpha = 0.85, size = 1.8, stroke = 0), 
        dpi = 300, dev = "ragg"
      ) +  
      # 彩色分组色块：在y=0位置添加彩色横条，标识数据库
      geom_rect(data = banner_df, 
                aes(xmin = x - 0.5, xmax = x + 0.5, ymin = -banner_height, ymax = banner_height, fill = Database),
                color = "grey40", linewidth = 0.2, alpha = 1, inherit.aes = FALSE) +
      
      # 横条上的标准名称文本
      geom_text(data = banner_df, 
                aes(x = x, y = 0, label = label), 
                color = "black", fontface = "bold", size = 2.8, inherit.aes = FALSE) +
      
      # 颜色标度设置
      scale_fill_manual(values = banner_colors, guide = "none") +  
      # 显式展示灰色图例
      scale_color_manual(values = c("Up" = "#E41A1C", "Down" = "#014AC7", "NotSig" = "grey75"),  
                         labels = c("Up" = "Up-regulated", "Down" = "Down-regulated", "NotSig" = "Not Significant"),
                         name = "Significance") +
      
      # 🎯【核心优化处】：强制固定 y 轴文本字符宽度，确保多个组别的火山图面板宽度精确一致
      scale_y_continuous(
        limits = c(-y_limit, y_limit), 
        expand = expansion(mult = c(0.02, 0.02)),
        labels = function(x) ifelse(is.na(x), "", sprintf("%6.1f", x)) # 格式化为右对齐，总宽6位保留1位小数
      ) +
      
      # x轴设置。强制紧缩左右白边，使背景条纹边缘完美接合面板边框。
      scale_x_continuous(breaks = 1:length(databases), 
                         labels = databases,
                         limits = c(0.5, length(databases) + 0.5), # 限定绘图区精确对应 bg_df 范围
                         expand = c(0, 0)) +                       # 移除坐标轴左右两侧的自动扩展空白
      
      # 上调标签
      geom_text_repel(data = top_labels_up, 
                      aes(x = jitter_x, y = log2FoldChange, label = Label),
                      size = 2.0, fontface = "italic", color = "black",
                      min.segment.length = 0,          
                      segment.color = "grey40",        
                      segment.size = 0.3,              
                      point.padding = 0.1,             # 避免线段切入点中心
                      box.padding = 0.4,               # 增大标签之间的排斥空间
                      direction = "both",              
                      nudge_y = y_limit * 0.05,        # 统一给一个向上的推力
                      force = 5.0,                     # 增强排斥力，使布局更发散
                      max.overlaps = Inf,              # 强制绘制所有标签，不隐藏
                      ylim = c(banner_height * 1.5, y_limit * 0.95),  # 限制标签不遮挡横幅并控制在顶部边界内
                      show.legend = FALSE) +
      
      # 下调标签
      geom_text_repel(data = top_labels_down, 
                      aes(x = jitter_x, y = log2FoldChange, label = Label),
                      size = 2.0, fontface = "italic", color = "black",
                      min.segment.length = 0,          
                      segment.color = "grey40",        
                      segment.size = 0.3,              
                      point.padding = 0.1,             
                      box.padding = 0.4,               
                      direction = "both",              
                      nudge_y = -y_limit * 0.05,       # 统一给一个向下的推力
                      force = 5.0,                     
                      max.overlaps = Inf,              
                      ylim = c(-y_limit * 0.95, -banner_height * 1.5),  # 限制标签不遮挡横幅并控制在底部边界内
                      show.legend = FALSE) +
      
      # 全局主题适配 - 定制图表样式
      theme_bw(base_size = 9) +  
      theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5, color = "#333333", margin = margin(b=4)),
        axis.title.y = element_text(size = 10, face = "bold", color = "black"),
        axis.title.x = element_text(size = 10, face = "bold", color = "black", margin = margin(t = 4)),
        axis.text.x = element_blank(),  # 隐藏x轴刻度文字（因为有横条标签）
        axis.ticks.x = element_blank(), # 隐藏x轴刻度线
        axis.text.y = element_text(size = 10, color = "black"),
        panel.grid.major = element_blank(),  # 移除主要网格线
        panel.grid.minor = element_blank(),  # 移除次要网格线
        panel.border = element_rect(color = "black", linewidth = 0.8),  # 设置边框
        legend.position = "right",  # 图例位置
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.3, "cm"),
        legend.background = element_rect(fill = "transparent")
      ) +
      # 强制放大图例散点尺寸，便于观察灰色方块
      guides(color = guide_legend(override.aes = list(size = 2.5))) +
      
      # 添加标题和轴标签
      labs(
        title = paste0("Multi-group Volcano Plot (", Keyword, ")"),
        x = "Functional Databases / Clusters",
        y = expression(Log[2] * " (Fold Change)")
      )
    
    # 构建输出文件前缀路径
    out_prefix <- file.path(funcunt_Dir, paste0("MultiGroup_Volcano_Banner_", Keyword))
    
    # 保存图表为PDF和PNG格式
    ggsave(paste0(out_prefix, ".pdf"), plot = p, width = 8.1, height = 3, dpi = 300)
    ggsave(paste0(out_prefix, ".png"), plot = p, width = 8.1, height = 3, dpi = 300)
    
    # 输出保存成功信息
    cat("   - 图表已保存为 PDF 与 PNG 格式 (8.1 x 1.7 英寸)。\n")
  }
}

# 输出脚本完成信息
cat("\n✅ 所有分组联合火山图宽幅比例 (防遮挡优化且面板宽度对齐) 完美适配完毕！\n")







##### Figure 4E####

rm(list = ls()) 
gc()        


suppressMessages({
  library(data.table)  # 提供极速的大数据表读写
  library(KEGGREST)    # KEGG在线映射
  library(dplyr)       # 数据框操作
  library(tidyr)       # 补全数据网格
  library(ggplot2)     # 可视化出图
})

# [路径规范] 
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/4_metadata_analysis/")
funcunt_Dir <- "./2_analysis_R/2_Gene-path/11_Fun-reduncance_Macro_Micro-4"
out_global_dir <- file.path(funcunt_Dir, "Global_Cross_Database_Summary")
if(!dir.exists(out_global_dir)) dir.create(out_global_dir, recursive = TRUE)


cat("\n🌐 正在读入合并数据并渲染全局贡献度景观热图...\n")

# 指定刚才导出的文件路径
raw_data_path <- file.path(out_global_dir, "PlotData_Pre6C_All_Databases_Raw_Contribution.csv")


if(file.exists(raw_data_path)) {
  global_micro <- fread(raw_data_path)
  
  # 1. 数据汇总
  global_micro_summary <- global_micro %>% 
    group_by(Database, Taxonomic_Unit, Group) %>% 
    summarise(Overall_Relative_Pct = mean(Mean_Contribution_Pct, na.rm=TRUE), .groups="drop")
  
  # 2. 显著性检验与上下调判断
  sig_list <- list()
  for(db in unique(global_micro$Database)) {
    for(tax in unique(global_micro$Taxonomic_Unit)) {
      
      sub_df <- global_micro[global_micro$Database == db & global_micro$Taxonomic_Unit == tax, ]
      syn_vals <- sub_df$Mean_Contribution_Pct[sub_df$Group == "Syn"]
      
      for(g in unique(sub_df$Group)) {
        if(g == "Syn") {
          sig_list[[length(sig_list)+1]] <- data.frame(Database=db, Taxonomic_Unit=tax, Group=g, Sig="", Trend="None")
        } else {
          g_vals <- sub_df$Mean_Contribution_Pct[sub_df$Group == g]
          
          if(length(g_vals) >= 2 && length(syn_vals) >= 2 && sd(c(g_vals, syn_vals), na.rm=TRUE) > 1e-8) {
            pval <- t.test(g_vals, syn_vals, var.equal=FALSE)$p.value
            mark <- ifelse(pval < 0.001, "***", ifelse(pval < 0.01, "**", ifelse(pval < 0.05, "*", "")))
            
            if(pval < 0.05) {
              trend <- ifelse(mean(g_vals, na.rm=TRUE) > mean(syn_vals, na.rm=TRUE), "Up", "Down")
            } else {
              trend <- "None"
            }
          } else {
            mark <- ""
            trend <- "None"
          }
          sig_list[[length(sig_list)+1]] <- data.frame(Database=db, Taxonomic_Unit=tax, Group=g, Sig=mark, Trend=trend)
        }
      }
    }
  }
  sig_df <- rbindlist(sig_list)
  
  # 3. 数据合并与因子对齐
  global_micro_summary <- merge(global_micro_summary, sig_df, by=c("Database", "Taxonomic_Unit", "Group"), all.x=TRUE)
  
  global_micro_summary$Group <- factor(global_micro_summary$Group, levels = target_levels) 
  global_micro_summary$Taxonomic_Unit <- factor(global_micro_summary$Taxonomic_Unit, levels = names(taxa_colors)) 
  global_micro_summary$Database <- factor(global_micro_summary$Database, levels = rev(db_order)) 
  
  global_micro_summary <- global_micro_summary %>% tidyr::complete(Database, Taxonomic_Unit, Group)
  
  # 4. 优化绘图
  p_global_heatmap <- ggplot(global_micro_summary, aes(x = Group, y = Database, fill = Overall_Relative_Pct)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = Sig, color = Trend), size = 4, fontface = "bold", vjust = 0.75, na.rm = TRUE) +
    scale_color_manual(
      values = c("Up" = "red", "Down" = "grey30", "None" = "transparent"),
      na.value = "transparent", guide = "none" 
    ) +
    scale_fill_gradientn(
      colors = c( "#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494"), 
      name = "Relative\nContribution (%)",
      limits = c(0, max(global_micro_summary$Overall_Relative_Pct, na.rm=TRUE)), 
      na.value = "grey85" 
    ) +
    facet_wrap(~ Taxonomic_Unit, nrow = 1) + 
    theme_bw() +
    labs(title = "The Global Blueprint: Target Genera Contribution Across Functional Databases", x = "Treatments", y = "Functional Databases") +
    theme(
      strip.text = element_text(size=9, face="bold.italic", margin = margin(t = 2, b = 2, unit = "pt")), 
      axis.text.x = element_text(size=9, face="bold"), 
      axis.text.y = element_text(size=9, face="bold"), 
      panel.grid = element_blank(), 
      legend.position = "right",
      strip.background = element_rect(fill = "grey90", color = "black"),
      panel.background = element_rect(fill = "white", color = NA) 
    )
  
  write.csv(global_micro_summary, file.path(out_global_dir, "PlotData_6C_Global_Genera_Contribution.csv"), row.names = FALSE)
  ggsave(file.path(out_global_dir, "6C_Plot_Global_Genera_Contribution_Heatmap.pdf"), plot=p_global_heatmap, width=9.5, height=1.9)
  ggsave(file.path(out_global_dir, "6C_Plot_Global_Genera_Contribution_Heatmap.png"), plot=p_global_heatmap, width=9.5, height=1.9, dpi=300, bg="white")
  
  cat("✅ 成功！全局贡献度景观热图 (图 6C) 已生成并保存完毕。\n")
} else {
  cat("❌ 错误：未能找到导出的原始数据，请检查文件路径！\n")
}



##### Figure 4F####



rm(list = ls()) 
gc()  
setwd("D:/4_小麦抗旱项目/14-实验/")

# 加载必要包，若未安装请先 install.packages(c("tidyverse", "ggsci", "viridis"))
library(tidyverse)
library(ggsci)     # 用于 NPG 等学术期刊配色
library(viridis)   # 用于 Plasma 连续离散色系
library(ggrepel) # 用于 Plot 3 的防遮挡直接文本标签
library(scales)  # 用于规范坐标轴数字格式



data0 <- read.table("./drew-data_pse.txt", header = TRUE, sep = "\t", check.names = FALSE)

data_long <- data0 %>%
  pivot_longer(
    cols = matches("^[0-9]+h$"), 
    names_to = "Time_chr", 
    values_to = "OD"
  ) %>%
  mutate(Time = as.numeric(gsub("h", "", Time_chr)))


blanks <- data_long %>%
  filter(group %in% c("Bac_BLANK", "Bac.SN_BLANK")) %>%
  select(group, Time, Blank_OD = OD) %>%
  mutate(Target_Group = if_else(group == "Bac_BLANK", "Bac", "Bac.SN")) %>%
  select(Target_Group, Time, Blank_OD)

data_adj <- data_long %>%
  filter(!group %in% c("Bac_BLANK", "Bac.SN_BLANK")) %>%
  left_join(blanks, by = c("group" = "Target_Group", "Time" = "Time")) %>%
  mutate(OD_adj = OD - Blank_OD)


data_mean <- data_adj %>%
  group_by(Group2, group, Time) %>%
  summarise(
    Mean_OD = mean(OD_adj, na.rm = TRUE),
    SE_OD = sd(OD_adj, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )


p1 <- ggplot(data_mean, aes(x = Time, y = Mean_OD, color = group, fill = group)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray85", linewidth = 0.8) + # 新增：0 OD辅助线
  geom_line(linewidth = 0.8) +
  geom_ribbon(aes(ymin = Mean_OD - SE_OD, ymax = Mean_OD + SE_OD), alpha = 0.2, color = NA) + 
  facet_wrap(~ Group2, scales = "free_y", ncol = 4) +
  theme_bw(base_size = 14) +
  # 新增：映射图例名称，使其更具学术可读性
  scale_color_npg(labels = c("Bac" = "Control (CK)", "Bac.SN" = "+ Pse Supernatant")) +
  scale_fill_npg(labels = c("Bac" = "Control (CK)", "Bac.SN" = "+ Pse Supernatant")) +
  scale_x_continuous(breaks = seq(0, 70, by = 10)) + 
  # 统一控制 Y 轴最多显示两位小数，避免刻度过长
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) + 
  labs(
    title = "Growth Dynamics of 13 Bacterial Strains",
    subtitle = "Treatment (Pse Supernatant) vs Control",
    x = "Incubation Time (h)", 
    y = expression(Adjusted~OD[600]),
    color = "Treatment:", # 调整图例大标题
    fill = "Treatment:"
  ) +
  theme(
    axis.title.x = element_text(size = 14, color = "black", face = "bold"),
    axis.title.y = element_text(size = 14, color = "black", face = "bold"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold.italic", size = 14, margin = margin(t = 8, b = 8, unit = "pt")),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    panel.grid.minor = element_blank()
  )
print(p1)




data_net <- data_mean %>%
  select(Group2, group, Time, Mean_OD) %>%
  pivot_wider(names_from = group, values_from = Mean_OD) %>%
  mutate(Net_OD = Bac.SN - Bac)

p2 <- ggplot(data_net, aes(x = Time, y = Net_OD)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 0.8) +
  geom_line(color = "#00A087FF", linewidth = 0.8) + 
  facet_wrap(~ Group2, scales = "free_y", ncol = 4) +
  theme_bw(base_size = 14) +
  scale_x_continuous(breaks = seq(0, 70, by = 10)) +
  labs(
    title = "Net Growth Kinetics by Strain",
    subtitle = expression(Net~OD[600] == OD[Bac.SN] - OD[Bac]),
    x = "Incubation Time (h)", 
    y = expression(Net~OD[600])
  ) +
  theme(
    axis.title.x = element_text(size = 14, color = "black", face = "bold"),
    axis.title.y = element_text(size = 14, color = "black", face = "bold"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    strip.background = element_rect(fill = "white", color = "black", linewidth = 0.8),
    strip.text = element_text(face = "bold.italic", size = 12),
    panel.grid.minor = element_blank()
  )
print(p2)


p3 <- ggplot(data_net, aes(x = Time, y = Net_OD, color = Group2, group = Group2)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60", linewidth = 1) +
  geom_line(linewidth = 0.8, alpha = 0.8) +
  theme_bw(base_size = 14) +
  scale_color_viridis_d(option = "plasma") + 
  scale_x_continuous(breaks = seq(0, 70, by = 10)) +
  labs(
    title = "Summary of Net Growth Across Strains",
    subtitle = "Time-series net response to Pse supernatant",
    x = "Incubation Time (h)", 
    y = expression(Net~OD[600]), 
    color = "Bacterial Strain"
  ) +
  theme(
    axis.title.x = element_text(size = 14, color = "black", face = "bold"),
    axis.title.y = element_text(size = 14, color = "black", face = "bold"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    legend.position = "right",
    legend.text = element_text(face = "italic", size = 11),
    panel.grid.minor = element_blank()
  )
print(p3)


ggsave("Plot1_Growth_Curves.pdf", p1, width = 10, height = 9, device = cairo_pdf)
ggsave("Plot2_Net_OD_Faceted.pdf", p2, width = 12, height = 9, device = cairo_pdf)
ggsave("Plot3_Net_OD_Summary.pdf", p3, width = 6, height = 3.5, device = cairo_pdf)

ggsave("Plot1_Growth_Curves.png", p1, width = 10, height = 9, dpi = 300)
ggsave("Plot2_Net_OD_Faceted.png", p2, width = 12, height = 9, dpi = 300)
ggsave("Plot3_Net_OD_Summary.png", p3, width = 6, height = 3.5, dpi = 300)











####******************************************************####


#### Figure 5 ####


##### Figure 5A ####

rm(list = ls()) 
gc()

# 加载必要的R包
library(data.table)  # 高效读取大文件
library(ape)         # 处理系统发育树的基础包
library(dplyr)       # 数据清洗与处理
library(tidyr)       # 数据长宽格式转换
library(stringr)     # 字符串处理
library(ggtree)      # 绘制系统发育树的核心包
library(ggtreeExtra) # 为 ggtree 添加外圈图层 (geom_fruit)
library(ggnewscale)  # 允许在同一个 ggplot 中使用多个 fill/color 映射标度
library(ggplot2)     # 基础绘图与主题设置
library(scales)

# 严格锁定项目根目录 (保留原机构名称)
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/4_metadata_analysis/")

# 定义并自动创建输出目录
out_DIR <- "./2_analysis_R/3_bin-path/7_phylo_function/"
if (!dir.exists(out_DIR)) dir.create(out_DIR, recursive = TRUE)


# 1. 数据加载与前处理


MAGs_TPM <- read.table("./2_analysis_R/3_bin-path/0_data/1_MAGs_TPM_Cleaned_95.tsv", header = T, sep = "\t")

if (!"Gene_ID" %in% colnames(MAGs_TPM)) {
  MAGs_TPM <- tibble::rownames_to_column(MAGs_TPM, var = "Gene_ID")
}


# 计算各组 (CK, H18, 等) 的 TPM 均值
TPM_grouped_means0 <- MAGs_TPM %>%
  pivot_longer(cols = -Gene_ID, names_to = "Sample", values_to = "TPM") %>%
  mutate(Group = str_remove(Sample, "_\\d+$")) %>% # 去除样本名后缀(如 _1, _2)得到组名
  
  mutate(Group = str_replace_all(Group, "H18", "S.Pse."),
         Group = str_replace_all(Group, "H20", "S.Sph.")) %>%
  group_by(Gene_ID, Group) %>%
  summarise(Mean_TPM = mean(TPM, na.rm = TRUE), .groups = "drop") %>% # 按组求均值
  pivot_wider(names_from = Group, values_from = Mean_TPM) # 转回宽表

colnames(TPM_grouped_means0)[1] <- "MAG_ID" # 统一列名用于后续合并


#  B. 分类与组装质量数据 
Taxon_Levels0 <- fread("./2_analysis_R/3_bin-path/0_data/3_Taxon_Levels_Completed_95.tsv", check.names = FALSE)
colnames(Taxon_Levels0)[1] <- "MAG_ID"

Prokka_Summary0 <- read.table("./1_data-analysis/8_分箱径向/4_annotation/10_statistics/Species_95_Prokka_Summary.tsv", header = T, sep = "\t") 

#  C. 进化树数据 
Phylo_Tree0 <- read.tree("./1_data-analysis/8_分箱径向/4_Taxonomy/infer_tree-95/gtdbtk.unrooted.tree")



rename_file <- read.table("./2_analysis_R/3_bin-path/0_data/3_Taxon_Levels_Completed_95_renamed.tsv", 
                          sep = "\t", header = TRUE, stringsAsFactors = FALSE)


name_map <- setNames(rename_file$new_bin_name, rename_file$user_genome)

#  2. 批量更新各数据对象中的 MAG_ID 


TPM_grouped_means <- TPM_grouped_means0 %>%
  mutate(MAG_ID = name_map[MAG_ID])


Taxon_Levels <- Taxon_Levels0 %>%
  mutate(MAG_ID = name_map[MAG_ID])


Prokka_Summary <- Prokka_Summary0 %>%
  mutate(MAG_ID = name_map[MAG_ID])

# D. 更新进化树末梢节点标签 (ape phylo 对象)
Phylo_Tree <- Phylo_Tree0
Phylo_Tree$tip.label <- name_map[Phylo_Tree0$tip.label]

#  D. 提取绘图所需的 3 个基础数据框 
# 【安全优化】：废弃不稳定的数字列号 c(1, 5, 2, 3, 4)，改用显式列名选取，杜绝位置错乱报错
drew_data1 <- TPM_grouped_means %>% dplyr::select(MAG_ID, Syn, CK, S.Pse., S.Sph.)
drew_data2 <- Prokka_Summary[, c(1, 3:8)]              # 组装质量矩阵: contigs, bases, CDS 等
drew_data3 <- Taxon_Levels[, c(1, 3)]                  # 物种分类矩阵: MAG_ID, Phylum



# 2. 格式转换与美学设置


heatmap_long0 <- drew_data1 %>%
  pivot_longer(
    cols = c("Syn", "CK", "S.Pse.", "S.Sph."), 
    names_to = "Sample", 
    values_to = "TPM"
  ) %>%

  mutate(Sample = factor(Sample, levels = c("Syn", "CK", "S.Pse.", "S.Sph.")))


# 确保在绘图前，heatmap_long 中包含 log10(TPM + 1) 列
heatmap_long1 <- heatmap_long0 %>%
  mutate(Log10_TPM = log10(TPM + 1))



# 定义 11 个 Phylum 的高辨识度离散配色方案
phylum_colors <- c(
  "#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", 
  "#80B1D3", "#FDB462", "#B3DE69", "#FCCDE5", 
  "#D9D9D9", "#BC80BD", "#CCEBC5"
)

bar_colors <- c(
  "contigs" = "#F28E8E", # 淡雅粉红 (Soft Rose)
  "bases"   = "#FBC486", # 浅杏橙色 (Pale Apricot)
  "CDS"     = "#8DB4D4", # 粉蓝/天青石色 (Powder Blue)
  "rRNA"    = "#BBA6DD", # 柔和浅紫 (Soft Violet)
  "tRNA"    = "#96D1A7", # 薄荷浅绿 (Mint Green)
  "tmRNA"   = "#F3A0CD"  # 浅樱花粉 (Light Cherry Pink)
)



# 3. 核心绘图流 (环形多层拼接)


#  图层 1: 基础环形进化树 + 门分类色块 

# 第一步：绘制基础的扇形树（设定60度缺口）
# 【核心修正】：将 color = length 修改为标准的 branch.length，使颜色完美映射枝长
p_base <- ggtree(Phylo_Tree, layout = "fan", open.angle = 90, branch.length = "none", 
                 color = "black", linewidth = 1.253)


# 第二步：【核心步】旋转这棵树
p_rotated <- rotate_tree(p_base, angle = 180)


p_rotated$data$angle <- p_rotated$data$angle %% 360

# 第三步：在旋转后的树上，添加外圈数据和标签
p1_tree <- p_rotated + 
  
  # 第1圈：门分类背景色块
  geom_fruit(
    data = drew_data3,
    geom = geom_tile,
    mapping = aes(y = MAG_ID, fill = Phylum),
    width = 16.4,        
    offset = 0.71,      
    alpha = 0.5        
  ) +
  
  # 用原生 geom_text 彻底接管末端标签排列
  geom_text(
    data = function(d) subset(d, isTip), # 仅提取树枝末梢节点画标签
    mapping = aes(
      x = x + 0.45,
      y = y,                               
      label = label,
      angle = angle                        
    ),
    color = "black",                     # 【关键修复】：强制外周文本为纯黑，防止继承全局 branch.length 的渐变变色
    size = 5.0,                              
    hjust = 0,                             
    fontface = "bold"
  ) +
  
  # 为进化树分支增加连续型美学配色（可更换为 "viridis" 或 "magma"）
  # scale_color_viridis_c(option = "plasma", name = "Branch Length") + 
  scale_fill_manual(values = phylum_colors, name = "Phylum") +  
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
  ) +
  xlab(NULL) + ylab(NULL) + 
  xlim(0, NA)

cat("\n[可视化调试] 正在打印图层 1: 基础树与分类标签...\n")
print(p1_tree)



#  图层 2: 组学丰度热图 (已修正图层顺序，放回内层) 

# 【修正说明】：必须在此处接在 p1_tree 后面！这样热图才会紧贴着内部的分类色块展示。

# 使用 ggtreeExtra 包的 geom_fruit() 在树图的右侧添加热图层
# 将树图对象 p1_tree 作为基础，在其上叠加热图




p2_heatmap <- p1_tree +
  

new_scale_fill() + 
  
 
geom_fruit(
  # 数据：长格式的热图数据框，包含 MAG_ID、Sample、TPM 三列
  data = heatmap_long1,
  # 几何对象：使用 tile（矩形色块）绘制热图
  geom = geom_tile,
  # 美学映射：y 轴为 MAG（与树图叶子对应），x 轴为样本，填充颜色为 log10(TPM + 1) 值
  mapping = aes(y = MAG_ID, x = Sample, fill = Log10_TPM), 
  
 
  offset = 0.38,       # 【优化】热图整体距离内部 Phylum 色带的间距，控制热图与树图主干的远近
  pwidth = 0.35,       # 【修复】原 0.40 过大，适度缩小至 0.55，避免热图区域过度拉伸
  width = 1.5,           # 恢复为标准默认值1
  height = 1,          # 恢复为标准默认值1，确保严格 1:1 继承 Y 轴树枝间距
  

  color = "white",     # 每个色块的边框颜色（白色增加区分度）
  linewidth = 0.3569,     # 边框粗细
  

  axis.params = list(
    axis = "x",        # 仅显示 x 轴（样本名称），y 轴为 MAG 名称，通常不显示以避免拥挤
    text.angle = 90,   # 【可调】列名文字旋转 90°（在环形树图中呈放射状，适合圆形布局）
    hjust = 0,         # 文字水平对齐方式（右对齐，使文字靠近坐标轴）
    text.size = 6    # 列名字体大小
  )
) +
  

# 使用渐变色，从浅蓝到粉紫，表示 TPM 丰度从低到高
scale_fill_gradientn(
  colours = c("#85D4F9", "#F2F9FF",  "#D23B79"),  # 颜色向量（低→高）
  # 【关键修改】：去除了 trans 参数，防止对已做对数转换的数据重复变换
  name = expression(log[10](TPM + 1)),         # 图例标题更新为 log10(TPM + 1)
  breaks = 0:5,                                # 对数后的刻度值（对应 1, 10, 100...）
  labels = c("0", "1", "2", "3", "4", "5")     # 刻度标签
)

# 输出调试信息
cat("\n[可视化调试] 正在打印图层 2: 组学丰度热图...\n")
# 打印热图对象，渲染图形
print(p2_heatmap)


# 4. 打印与保存

ggsave(file.path(out_DIR, "MAG_Phylo_Multilayer_Circular-95-1.pdf"),
       plot = p2_heatmap,
       height = 16,
       width = 16,
       dpi = 300)

ggsave(file.path(out_DIR, "MAG_Phylo_Multilayer_Circular-95-1.png"),
       plot = p2_heatmap,
       height = 16,
       width = 16,
       dpi = 300)




##### Figure 5B####

rm(list = ls()); gc()
# 加载必要包
pacman::p_load(data.table, tidyverse, pheatmap, ggrepel, ggsci, 
               FactoMineR, factoextra, viridis, patchwork, igraph, ggraph,
               ggpubr, tidytext, ggridges)

# 设置基础工作目录 (请确保该路径存在)
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/4_metadata_analysis/")
data_DIR <- "./1_data-analysis/8_分箱径向/5_gapseq-COBRApy-99/"
out_DIR  <- "./2_analysis_R/3_bin-path/12_gapseqCOBRApyMICOM/5_MICOM/"

# 若目录不存在则递归创建
if (!dir.exists(out_DIR)) dir.create(out_DIR, recursive = TRUE)

# 设置本节分析专属的绘图输出目录
out_DIR2  <- "./2_analysis_R/3_bin-path/12_gapseqCOBRApyMICOM/5_MICOM/1_growth/"
if (!dir.exists(out_DIR2)) dir.create(out_DIR2, recursive = TRUE)


# 读取更新后的重命名映射表 (直接赋值给 taxa_map_raw)
taxa_map_raw <- read.table("./2_analysis_R/3_bin-path/0_data/3_Taxon_Levels_Completed_99_renamed.tsv", 
                           sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# 读取 MICOM 结果
AllSaCGrowth <- fread(paste0(data_DIR, "2_MICOM/micom_results-all/All_Samples_Community_Growth.csv"), 
                      header = TRUE, sep = ",", quote = "")

AllSaCFeedingFluxes <- fread(paste0(data_DIR, "2_MICOM/micom_results-all/All_Samples_CrossFeeding_Fluxes.csv"), 
                             header = TRUE, sep = ",", quote = "")

# 兼容修复：统一列名为 compartment 以避免后续报错
if("compartments" %in% names(AllSaCGrowth)) {
  AllSaCGrowth <- rename(AllSaCGrowth, compartment = compartments)
}

# 读取本地化合物字典
metabolite_dict <- tryCatch({
  local_file_path <- "./2_analysis_R/3_bin-path/12_gapseqCOBRApyMICOM/0_data/compounds.tsv"
  seed_db <- fread(local_file_path, select = c("id", "name"), quote = "")
  setNames(seed_db$name, seed_db$id)
}, error = function(e) {
  cat("[WARNING] 本地文件读取失败，已使用内置备用字典。\n")
  c("cpd00009" = "Phosphate", "cpd00027" = "D-Glucose", "cpd00030" = "Mn2+",
    "cpd00034" = "Zn2+", "cpd00067" = "H+", "cpd00205" = "K+",
    "cpd00013" = "NH3", "cpd00036" = "Succinate", "cpd00029" = "Acetate")
})



# 1. 映射表预处理：同步更新 user_genome 标识符 (H18 -> S.Pse., H20 -> S.Sph.)
taxa_map_clean <- taxa_map_raw %>%
  mutate(
    user_genome = gsub("H18", "S.Pse.", user_genome),
    user_genome = gsub("H20", "S.Sph.", user_genome)
  )

# 2. 生长数据深度清洗与精确分组提取
growth_clean <- AllSaCGrowth %>%
  # 剔除培养基本底数据
  filter(compartment != "medium") %>%
  mutate(
    # 提取处理组名称前缀
    Group = gsub("_[0-9]+$", "", sample_id),
    # 替换特定组名
    Group = case_when(
      Group == "H18" ~ "S.Pse.", 
      Group == "H20" ~ "S.Sph.", 
      TRUE ~ Group
    ),
    # 同步更新 MAG ID (compartment)
    compartment = gsub("H18", "S.Pse.", compartment),
    compartment = gsub("H20", "S.Sph.", compartment)
  )

# 严格设定全局因子顺序
growth_clean$Group <- factor(growth_clean$Group, levels = c("Syn", "CK", "S.Pse.", "S.Sph."))

# 3. 核心修正：稀疏补零与组内聚合计算
mag_stats0 <- growth_clean %>%
  # 在单样本水平先计算真实的衍生适应度指标
  mutate(Sample_Fitness = abundance * growth_rate) %>%
  
  # 按组别补齐缺失的样本与 MAG 组合，缺失值强制填 0
  group_by(Group) %>%
  tidyr::complete(
    sample_id,
    compartment,
    fill = list(abundance = 0, growth_rate = 0, Sample_Fitness = 0)
  ) %>%
  ungroup() %>%
  
  # 补零闭环后进行组内均值聚合
  group_by(Group, compartment) %>%
  summarise(
    Mean_Abundance = mean(abundance, na.rm = TRUE),
    Mean_Growth    = mean(growth_rate, na.rm = TRUE),
    Fitness        = mean(Sample_Fitness, na.rm = TRUE), 
    .groups        = "drop"
  )

# 4. 统一 ID 格式并关联分类学注释表
mag_stats <- mag_stats0 %>%
  # 将 "bin_" 转换为 "bin."，使其与 user_genome 匹配
  mutate(user_genome = str_replace(compartment, "bin_", "bin.")) %>%
  
  # 关联预处理后的分类信息表
  left_join(taxa_map_clean, by = "user_genome") %>%
  
  # 整理列顺序（直接使用真实存在的列名 new_bin_name）
  select(
    Group,
    compartment_raw = compartment,  # 原始计算 ID (如 S.Pse._bin_10)
    user_genome,                    # 统一匹配 ID (如 S.Pse._bin.10)
    str_id,                         # 菌株编号 (如 str01)
    clean_taxon,                    # 菌属分类简称 (如 ZC4RG17)
    new_bin_name,                   # 【修正】本身就是列名，直接调用，不写等号重命名
    Kingdom, Phylum, Class, Order, Family, Genus, Species,
    Mean_Abundance,
    Mean_Growth,
    Fitness
  )


top6_abund   <- mag_stats %>% group_by(Group) %>% slice_max(Mean_Abundance, n = 4, with_ties = FALSE) %>% ungroup()
top6_growth  <- mag_stats %>% group_by(Group) %>% slice_max(Mean_Growth, n = 4, with_ties = FALSE) %>% ungroup()
top6_fitness <- mag_stats %>% group_by(Group) %>% slice_max(Fitness, n = 4, with_ties = FALSE) %>% ungroup()


abund_summary <- mag_stats %>%
  group_by(Group) %>%
  summarise(Avg_Abund = mean(Mean_Abundance, na.rm = TRUE), SE = sd(Mean_Abundance, na.rm = TRUE)/sqrt(n()))




p2_merged_bubble <- ggplot(mag_stats, aes(x = Mean_Abundance, y = Mean_Growth, color = Group)) +
  
 
geom_point(aes(size = Fitness, fill = Group), 
           shape = 21, color = "black", alpha = 0.7) +
  
 
geom_smooth(method = "lm", aes(fill = Group), 
            linetype = "dashed", alpha = 0.15) +
  
 
stat_cor(method = "spearman", size = 3, show.legend = FALSE, 
         label.x.npc = 0.65, label.y.npc = 0.35) +
  
 
geom_text_repel(data = top6_fitness, 
                aes(label = new_bin_name, color = Group), 
                size = 2.5, fontface = "italic", 
                max.overlaps = Inf, show.legend = FALSE) +
  

scale_fill_npg() + 
  scale_color_npg() +

  scale_size_continuous(range = c(1, 6)) +
  

labs(title = "2D Density and Fitness Bubble Plot (Merged)", 
     x = "Relative Abundance",          # 相对丰度
     y = "Growth Rate",                 # 生长速率
     size = "Fitness") +                # 图例标题

theme_bw() +                            # 使用黑白主题背景
  theme(

    legend.position = "right",

    axis.text = element_text(size = 12, color = "black"),

    axis.title = element_text(size = 14, face = "bold", color = "#2C3E50"),

    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),

    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey30"),

    legend.text = element_text(size = 11, color = "black"),

    legend.title = element_text(size = 12, face = "bold"),

    strip.background = element_rect(fill = "grey95", color = "grey80"),

    strip.text = element_text(size = 13, face = "bold"),

    panel.grid.minor = element_blank(),

    panel.border = element_rect(linewidth = 1, color = "black")
  )

ggsave(paste0(out_DIR2, "2_Coupling_Merged_Bubble.pdf"), p2_merged_bubble, width = 5, height = 3)
ggsave(paste0(out_DIR2, "2_Coupling_Merged_Bubble.png"), p2_merged_bubble, width = 5, height = 3, dpi = 300)



####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!####
#####Figure 5C, D#####


rm(list = ls())
gc()

pacman::p_load(
  data.table,   # 提供 fread() 超快速读取大型代谢流 CSV 矩阵
  tidyverse,    # 包含 dplyr（数据清洗）、ggplot2（绘图）、tidyr（矩阵整形）等
  pheatmap,     # 绘制群落通量热图（预留用于后续扩展）
  ggrepel,      # 为网络节点标签提供防重叠拉伸弹开功能
  ggsci,        # 提供符合 Nature、Science 等国际顶级期刊标准的科学配色
  RColorBrewer, # 经典学术调色板工具，用于生成连续型/离散型颜色向量
  FactoMineR,   # 多元统计学工具（PCA、PCoA分析），保留用于后续多组学关联
  factoextra,   # 配合 FactoMineR 实现学术级降维图谱可视化
  viridis,      # 色盲友好型连续色板
  patchwork,    # 使用快捷算子（+、/）无缝组合、编排多幅 ggplot 图形
  igraph,       # 图论网络分析核心包：构建图对象、计算出入度、HITS 拓扑指标
  ggraph,       # 基于 ggplot2 语法的网络高级可视化渲染包，与 igraph 无缝衔接
  agricolae     # 农业统计分析工具，用于 Tukey HSD 等多重比较
)


select <- dplyr::select
filter <- dplyr::filter
rename <- dplyr::rename
mutate <- dplyr::mutate


library(igraph)



calculate_scale_free_r2 <- function(g, mode = "all") {
 
  d <- degree(g, mode = mode)
  d <- d[d > 0]
  
  if (length(unique(d)) < 5) {
    return(list(is_scale_free = FALSE, alpha = NA, p_value = 0, message = "度阶层过少"))
  }
  

  fit <- fit_power_law(d, implementation = "plfit")
  
  # 3. 提取核心统计量
  alpha <- fit$alpha      # 幂律指数（通常在 2 到 3 之间具有物理意义）
  xmin  <- fit$xmin       # 拟合切入的最小度值
  ks_p  <- fit$KS.p       # KS检验的p值
  

  is_scale_free <- !is.na(ks_p) && ks_p >= 0.1 && alpha > 1 && alpha < 3.5
  
  return(list(
    mode = mode,
    is_scale_free = is_scale_free, # 是否通过现代科学检验
    alpha = alpha,                 # 幂律指数
    xmin = xmin,                   # 拟合下界
    ks_p_value = ks_p              # 拟合优度p值 (越大越好)
  ))
}



# 设置工作目录
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/4_metadata_analysis/")

data_DIR <- "./1_data-analysis/8_分箱径向/5_gapseq-COBRApy-99/"


out_DIR_Main <- "./2_analysis_R/3_bin-path/12_gapseqCOBRApyMICOM/5_MICOM/3_Streamlined_Macro_Modules/"
if (!dir.exists(out_DIR_Main)) dir.create(out_DIR_Main, recursive = TRUE)


out_DIR2 <- "./2_analysis_R/3_bin-path/12_gapseqCOBRApyMICOM/5_MICOM/1_growth/"
if (!dir.exists(out_DIR2)) dir.create(out_DIR2, recursive = TRUE)


my_theme <- theme_bw() +  # 黑白网格经典背景
  theme(
    axis.text = element_text(size = 12, color = "black"),                               # 轴刻度字号 12
    axis.title = element_text(size = 14, face = "bold", color = "#2C3E50"),             # 轴标题 14 加粗
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),                  # 主标题 16 居中加粗
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey30"),            # 副标题 12 居中深灰
    legend.text = element_text(size = 11, color = "black"),                             # 图例文字 11
    legend.title = element_text(size = 12, face = "bold"),                              # 图例标题 12 加粗
    strip.background = element_rect(fill = "grey95", color = "grey80"),                # 分面标签浅灰背景
    strip.text = element_text(size = 13, face = "bold"),                               # 分面标题 13 加粗
    panel.grid.minor = element_blank()                                                 # 清除次要网格线，降低画面噪声
  )


group_names <- c("Syn", "CK", "S.Pse.", "S.Sph.")

=====
# 第三部分：数据加载、ID替换与映射字典构建
=====

cat(">>> [1/6] 正在加载 MICOM 真实通量数据并执行全局标志符深度替换...\n")

#  3.1 读取群落物种比生长速率数据 
AllSaCGrowth <- fread(
  paste0(data_DIR, "2_MICOM/micom_results-all-refine/RhizoDroughtAllcondition_All_Samples_Community_Growth.csv"),
  quote = ""
) %>%
  mutate(
  
    compartment = gsub("H18", "S.Pse.", compartment),
    compartment = gsub("H20", "S.Sph.", compartment),
    sample_id   = gsub("H18", "S.Pse.", sample_id),
    sample_id   = gsub("H20", "S.Sph.", sample_id)
  )


mag_stats <- AllSaCGrowth %>%
  filter(compartment != "medium") %>%
  mutate(Group = gsub("_[0-9]+$", "", sample_id)) %>%
  mutate(Group = factor(Group, levels = group_names)) %>%
  # 【核心严谨性修正】在单样本层面计算真实的生物量协同适应度 (Sample_Fitness)
  mutate(Sample_Fitness = abundance * growth_rate) %>% 
  group_by(Group, compartment) %>%
  summarise(
    Mean_Abundance = mean(abundance, na.rm = TRUE),
    Mean_Growth    = mean(growth_rate, na.rm = TRUE),
    Mean_Fitness   = mean(Sample_Fitness, na.rm = TRUE), # 对衍生后的适应度求均值，避免数学逻辑错位
    .groups = "drop"
  ) %>%
  rename(Fitness = Mean_Fitness)

top6_abund   <- mag_stats %>% group_by(Group) %>% slice_max(Mean_Abundance, n = 6, with_ties = FALSE) %>% ungroup()
top6_growth  <- mag_stats %>% group_by(Group) %>% slice_max(Mean_Growth, n = 6, with_ties = FALSE) %>% ungroup()
top6_fitness <- mag_stats %>% group_by(Group) %>% slice_max(Fitness, n = 6, with_ties = FALSE) %>% ungroup()


AllSaCFeedingFluxes <- fread(
  paste0(data_DIR, "2_MICOM/micom_results-all-refine/RhizoDroughtAllcondition_All_Samples_CrossFeeding_Fluxes.csv"),
  quote = ""
) %>%
  mutate(
    MAG_ID    = gsub("H18", "S.Pse.", MAG_ID),
    MAG_ID    = gsub("H20", "S.Sph.", MAG_ID),
    sample_id = gsub("H18", "S.Pse.", sample_id),
    sample_id = gsub("H20", "S.Sph.", sample_id)
  )


metabolite_dict <- tryCatch({
  # 优先加载 gapseq 自带的标准化 compounds 映射文件
  seed_db <- fread(
    "./2_analysis_R/3_bin-path/12_gapseqCOBRApyMICOM/0_data/compounds.tsv",
    select = c("id", "name"), quote = ""
  )
  setNames(seed_db$name, seed_db$id)
}, error = function(e) {
  # 防御性快取：若本地字典损坏，启用紧急内置高频根系分泌物核心卡槽字典
  c("cpd00027"="D-Glucose", "cpd00082"="D-Fructose", "cpd00076"="Sucrose",
    "cpd00158"="Cellobiose", "cpd00003"="NAD", "cpd00009"="Phosphate")
})

#  3.5 核心科研设计：终极完全体小麦根际微生态宏观功能模块映射分类表 
functional_map <- c(
  # ===== 核心碳源与能量 =====
  "cpd00027" = "Core Carbon & Energy", "cpd00082" = "Core Carbon & Energy",
  "cpd00076" = "Core Carbon & Energy", "cpd00158" = "Core Carbon & Energy",
  "cpd00179" = "Core Carbon & Energy", "cpd00382" = "Core Carbon & Energy",
  "cpd00079" = "Core Carbon & Energy", "cpd00224" = "Core Carbon & Energy",
  "cpd00108" = "Core Carbon & Energy", "cpd00138" = "Core Carbon & Energy",
  "cpd00118" = "Core Carbon & Energy", "cpd00105" = "Core Carbon & Energy",
  "cpd05577" = "Core Carbon & Energy", "cpd00121" = "Core Carbon & Energy",
  "cpd00100" = "Core Carbon & Energy", "cpd00071" = "Core Carbon & Energy",
  "cpd00363" = "Core Carbon & Energy", "cpd00221" = "Core Carbon & Energy",
  "cpd01861" = "Core Carbon & Energy", "cpd00453" = "Core Carbon & Energy",
  "cpd00116" = "Core Carbon & Energy", "cpd00448" = "Core Carbon & Energy",
  "cpd00024" = "Core Carbon & Energy", "cpd00047" = "Core Carbon & Energy",
  "cpd11606" = "Core Carbon & Energy", "cpd11451" = "Core Carbon & Energy",
  "cpd00154" = "Core Carbon & Energy", 
  "cpd00306" = "Core Carbon & Energy", 
  "cpd00185" = "Core Carbon & Energy", 
  "cpd03662" = "Core Carbon & Energy", 
  "cpd00176" = "Core Carbon & Energy", 
  "cpd00055" = "Core Carbon & Energy", 
  "cpd00080" = "Core Carbon & Energy", 
  "cpd00208" = "Core Carbon & Energy", 
  "cpd00371" = "Core Carbon & Energy", 
  
  # ===== 根系分泌酸与微量营养 =====
  "cpd00130" = "Rhizosphere Exuded Acids & Nutrition", "cpd00137" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00036" = "Rhizosphere Exuded Acids & Nutrition", "cpd00029" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00159" = "Rhizosphere Exuded Acids & Nutrition", "cpd00020" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00106" = "Rhizosphere Exuded Acids & Nutrition", "cpd00032" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00023" = "Rhizosphere Exuded Acids & Nutrition", "cpd00038" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00033" = "Rhizosphere Exuded Acids & Nutrition", "cpd00003" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00009" = "Rhizosphere Exuded Acids & Nutrition", "cpd00013" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00209" = "Rhizosphere Exuded Acids & Nutrition", "cpd00048" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00035" = "Rhizosphere Exuded Acids & Nutrition", "cpd00041" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00322" = "Rhizosphere Exuded Acids & Nutrition", "cpd00107" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00119" = "Rhizosphere Exuded Acids & Nutrition", "cpd00666" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00180" = "Rhizosphere Exuded Acids & Nutrition", "cpd00054" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00053" = "Rhizosphere Exuded Acids & Nutrition", "cpd00084" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00039" = "Rhizosphere Exuded Acids & Nutrition", "cpd00246" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00211" = "Rhizosphere Exuded Acids & Nutrition", "cpd00644" = "Rhizosphere Exuded Acids & Nutrition",
  "cpd00161" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd01217" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00139" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00141" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00222" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00309" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00182" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00117" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00550" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00156" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00064" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00132" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00051" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd03847" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00573" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00073" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00307" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00311" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00367" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00249" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00133" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd01741" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00184" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00305" = "Rhizosphere Exuded Acids & Nutrition", 
  "cpd00232" = "Rhizosphere Exuded Acids & Nutrition", 
  
  # ===== 旱灾胁迫渗透压保护剂 =====
  "cpd00129" = "Drought-Stress Osmoprotectants", "cpd00092" = "Drought-Stress Osmoprotectants",
  "cpd00793" = "Drought-Stress Osmoprotectants", "cpd00314" = "Drought-Stress Osmoprotectants",
  "cpd00152" = "Drought-Stress Osmoprotectants", 
  "cpd00794" = "Drought-Stress Osmoprotectants", 
  "cpd00588" = "Drought-Stress Osmoprotectants", 
  "cpd00324" = "Drought-Stress Osmoprotectants", 
  "cpd00281" = "Drought-Stress Osmoprotectants", 
  "cpd00264" = "Drought-Stress Osmoprotectants", 
  "cpd01092" = "Drought-Stress Osmoprotectants", 
  
  # ===== 宿主植物激素信号传导 =====
  "cpd00325" = "Host Phytohormone Signaling", "cpd00523" = "Host Phytohormone Signaling",
  "cpd03371" = "Host Phytohormone Signaling", "cpd03233" = "Host Phytohormone Signaling",
  "cpd04085" = "Host Phytohormone Signaling", "cpd00326" = "Host Phytohormone Signaling",
  "cpd00296" = "Host Phytohormone Signaling", "cpd00216" = "Host Phytohormone Signaling", 
  "cpd00060" = "Host Phytohormone Signaling", "cpd00239" = "Host Phytohormone Signaling",
  "cpd00065" = "Host Phytohormone Signaling", 
  "cpd00359" = "Host Phytohormone Signaling", 
  "cpd00418" = "Host Phytohormone Signaling", 
  "cpd00204" = "Host Phytohormone Signaling", 
  "cpd00025" = "Host Phytohormone Signaling", 
  "cpd01981" = "Host Phytohormone Signaling", 
  
  # ===== 防御性生物抗性与次生酚类物质 =====
  "cpd11527" = "Defense & Secondary Phenolics", "cpd11634" = "Defense & Secondary Phenolics",
  "cpd00424" = "Defense & Secondary Phenolics", "cpd00684" = "Defense & Secondary Phenolics",
  "cpd00220" = "Defense & Secondary Phenolics", "cpd03155" = "Defense & Secondary Phenolics",
  "cpd00136" = "Defense & Secondary Phenolics", "cpd00823" = "Defense & Secondary Phenolics",
  "cpd00856" = "Defense & Secondary Phenolics", "cpd00399" = "Defense & Secondary Phenolics",
  "cpd00854" = "Defense & Secondary Phenolics", "cpd00744" = "Defense & Secondary Phenolics",
  "cpd00877" = "Defense & Secondary Phenolics", "cpd00340" = "Defense & Secondary Phenolics",
  "cpd00380" = "Defense & Secondary Phenolics",
  "cpd01553" = "Defense & Secondary Phenolics", 
  "cpd00150" = "Defense & Secondary Phenolics", 
  "cpd00489" = "Defense & Secondary Phenolics", 
  "cpd00066" = "Defense & Secondary Phenolics", 
  "cpd00069" = "Defense & Secondary Phenolics" 
)

#  3.6 环境无机物背景噪声及模型伪产物黑名单（加入微量求解器噪声，清洗大盘） 
inorganic_blacklist <- c(
  "cpd00001", "cpd00007", "cpd00011", "cpd00067", "cpd11640", 
  "cpd00012", 
  "cpd10515", "cpd10516", 
  "cpd00205", "cpd00030", "cpd00254", "cpd00058", "cpd00099", "cpd00149", "cpd00034", "cpd00063", 
  "cpd00659", "cpd00081", 
  "cpd11416", 
  "cpd00006", 
  "cpd00010", 
  "cpd00028", 
  "cpd00017", 
  "cpd00408", 
  "cpd00393", 
  "cpd00229", 
  "cpd15522"  
)

#  3.7 核心目标分箱基因组拓扑锁定白名单 
key_bin <- c("S.Pse._bin_10", "S.Sph._bin_5", "S.Sph._bin_21")


# 第四部分：通量数据清洗、过滤与功能分类


cat(">>> [2/6] 执行数据过滤与功能映射系统分配...\n")

translate_metabolites <- function(ids) {
  clean_ids <- gsub("^EX_|_[a-zA-Z0-9]+$", "", ids)
  trans <- metabolite_dict[clean_ids]
  trans[is.na(trans)] <- clean_ids[is.na(trans)]
  return(unname(trans))
}

# 清洗原始通量长表并剔除黑名单无机噪声
flux_all_organic <- AllSaCFeedingFluxes %>%
  mutate(
    Treatment = factor(gsub("_[0-9]+$", "", sample_id), levels = group_names),
    Metabolite_Clean = gsub("^EX_|_[a-zA-Z0-9]+$", "", metabolite),
    Metabolite_Name = translate_metabolites(metabolite)
  ) %>%
  filter(!Metabolite_Clean %in% inorganic_blacklist) %>%
  mutate(
    MacroCategory = if_else(
      Metabolite_Clean %in% names(functional_map),
      functional_map[Metabolite_Clean],
      "Other"
    )
  )

# 双路匹配设计：将明细类别与总体大盘类别合并，实现一个 for 循环内同步渲染个体与综合结果
flux_mapped <- flux_all_organic %>% filter(MacroCategory != "Other")
flux_all_combined <- flux_mapped %>% mutate(MacroCategory = "All_Mapped_Organics")
flux_unmapped <- flux_all_organic %>% filter(MacroCategory == "Other") %>% mutate(MacroCategory = "All_Unmapped_Organics")

flux_final_master <- bind_rows(flux_mapped, flux_all_combined, flux_unmapped)

# 【核心统计优化】在计算平均值前，使用 complete 对缺失的稀疏结合单元强行补零，消解幸存者偏差
actual_mean_fluxes <- flux_final_master %>%
  group_by(Treatment) %>%
  tidyr::complete(
    sample_id, 
    nesting(MAG_ID, Metabolite_Name, MacroCategory), 
    fill = list(flux = 0)
  ) %>%
  ungroup() %>%
  group_by(Treatment, MAG_ID, Metabolite_Name, MacroCategory) %>%
  summarise(
    Net_Flux        = mean(flux, na.rm = TRUE),     
    Gross_Intensity = mean(abs(flux), na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  filter(Gross_Intensity > 1e-4) # 过滤求解器极低数值误差死连接

# 自动导出未分配代谢物流明细诊断表，方便后期根据需要补充生态映射字典卡槽
fwrite(
  flux_unmapped %>%
    group_by(Metabolite_Clean, Metabolite_Name) %>%
    summarise(Total_Flux = sum(abs(flux)), .groups = "drop") %>%
    arrange(desc(Total_Flux)),
  paste0(out_DIR_Main, "0_Diag_Unmapped_Mets.csv")
)


# 第五部分：主分析循环——按功能模块逐一切片，渲染四种图形

cat(">>> [3/6] 启动超级全景切片渲染循环 (中心-外围锚定布局与图例完美一键归流)...\n")

QUANTILE_CUT <- 0.15

all_loop_categories <- unique(flux_final_master$MacroCategory)
color_palette <- setNames(scales::hue_pal()(length(all_loop_categories)), all_loop_categories)


# 读取更新后的重命名映射表 (直接赋值给 taxa_map_raw)
taxa_map_raw <- read.table("./2_analysis_R/3_bin-path/0_data/3_Taxon_Levels_Completed_99_renamed.tsv", 
                           sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# 2. 构建映射桥梁：精准匹配通量数据表中的 MAG_ID 格式
taxa_map_match <- taxa_map_raw %>%
  mutate(
    # 模拟通量数据中的命名规则生成 match_id 保证 100% 对齐匹配
    match_id = user_genome %>%
      gsub("\\.", "_", .) %>%            # 替换点号为下划线 (如 bin.39 -> bin_39)
      gsub("H18", "S.Pse.", .) %>%       # 替换组别 H18 为 S.Pse.
      gsub("H20", "S.Sph.", .)           # 替换组别 H20 为 S.Sph.
  ) %>%
  select(match_id, new_bin_name)


all_loop_categories <- c("All_Mapped_Organics")
#  5.3 主业务大循环启动 
for (current_macro in all_loop_categories) {
  

  safe_name <- gsub("[^A-Za-z0-9]+", "_", current_macro)
  dir_macro <- paste0(out_DIR_Main, safe_name, "/")
  if (!dir.exists(dir_macro)) dir.create(dir_macro, recursive = TRUE)
  
  cat(paste("\n==========\n"))
  cat(paste("    -> 正在全方位渲染生态/总汇模块:", current_macro, "\n"))
  cat(paste("==========\n"))
  
  max_fitness <- max(mag_stats$Fitness, na.rm = TRUE)
  
  macro_group_data0 <- flux_final_master %>% filter(MacroCategory == current_macro)
  macro_dyn_data0   <- actual_mean_fluxes %>% filter(MacroCategory == current_macro)
  cat_color        <- color_palette[current_macro]
  
  
  # 4. 彻底更新 macro_group_data 中的 MAG_ID
  macro_group_data <- macro_group_data0 %>%
    left_join(taxa_map_match, by = c("MAG_ID" = "match_id")) %>%
    # 替换为新的易读标签，若未能匹配则保留原 ID 防报错
    mutate(MAG_ID = ifelse(!is.na(new_bin_name), new_bin_name, MAG_ID)) %>%
    select(-new_bin_name) # 清理辅助列，保持原表列结构纯净
  
  # 5. 彻底更新 macro_dyn_data 中的 MAG_ID
  macro_dyn_data <- macro_dyn_data0 %>%
    left_join(taxa_map_match, by = c("MAG_ID" = "match_id")) %>%
    mutate(MAG_ID = ifelse(!is.na(new_bin_name), new_bin_name, MAG_ID)) %>%
    select(-new_bin_name) # 清理辅助列，保持原表列结构纯净
  
  
  
  
  
  if (is.na(cat_color)) cat_color <- "#2C3E50"
  
  # 防御性沙盘：若模块无数据，自动垫底生成空 PDF 闪避终止
  if (nrow(macro_group_data) == 0) {
    p_empty <- ggplot() + theme_void() +
      geom_text(aes(x = 0, y = 0), label = paste("NO VALID FLUX DATA DETECTED\nModule:", current_macro),
                size = 5, color = "grey50", fontface = "bold") +
      theme(plot.background = element_rect(fill = "white", color = NA))
    
    ggsave(paste0(dir_macro, "1_Bar.pdf"), p_empty, width = 6, height = 5, bg = "white")
    ggsave(paste0(dir_macro, "1_Bar.png"), p_empty, width = 6, height = 5, dpi = 300, bg = "white")
    ggsave(paste0(dir_macro, "2_SourceSink.pdf"), p_empty, width = 12, height = 6, bg = "white")
    ggsave(paste0(dir_macro, "2_SourceSink.png"), p_empty, width = 12, height = 6, dpi = 300, bg = "white")
    ggsave(paste0(dir_macro, "3_All_Bip.pdf"), p_empty, width = 15, height = 12, bg = "white")
    ggsave(paste0(dir_macro, "3_All_Bip.png"), p_empty, width = 15, height = 12, dpi = 300, bg = "white")
    ggsave(paste0(dir_macro, "4_All_Uni.pdf"), p_empty, width = 15, height = 12, bg = "white")
    ggsave(paste0(dir_macro, "4_All_Uni.png"), p_empty, width = 15, height = 12, dpi = 300, bg = "white")
    next   
  }
  

 
  
===
  # 源-汇景观图多方法全集成脚本（指定对照组：Syn）
===
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tibble)
  library(data.table)
  library(limma)       # 思路二依赖
  library(DescTools)   # Dunnett依赖
  library(FSA)         # Dunn依赖
  
  # -
  # STEP 1~4: 基础数据准备与高活性 Top 40 筛选（公共部分）
  # -
  
  # 1. 计算样本级别的净通量
  sample_level_flux <- macro_group_data %>%
    group_by(Treatment, sample_id, Metabolite_Name) %>%   
    summarise(Community_Net_Flux = sum(flux), .groups = "drop")   
  
  # 2. 计算各处理组的汇总统计量
  sink_source_summary <- sample_level_flux %>%
    group_by(Treatment, Metabolite_Name) %>%             
    summarise(
      Mean_Flux = mean(Community_Net_Flux, na.rm = TRUE),   
      SD_Flux   = sd(Community_Net_Flux, na.rm = TRUE),      
      N         = n(),                                     
      .groups   = "drop"                                   
    ) %>%
    mutate(
      SD_Flux = if_else(is.na(SD_Flux), 0, SD_Flux),       
      SE_Flux = SD_Flux / sqrt(N)                          
    )
  
  # 3. 基于样本绝对值之和筛选前40种高活性代谢物（防止组内正负抵消漏筛）
  met_order <- sample_level_flux %>%
    mutate(Abs_Flux = abs(Community_Net_Flux)) %>%         
    group_by(Metabolite_Name) %>%                          
    summarise(W = sum(Abs_Flux, na.rm = TRUE)) %>%         
    arrange(W) %>% tail(30) %>%                          
    pull(Metabolite_Name)                                  
  
  # 4. 过滤数据
  sample_level_top40 <- sample_level_flux %>% filter(Metabolite_Name %in% met_order)
  sink_source_top40  <- sink_source_summary %>% filter(Metabolite_Name %in% met_order)
  
  

  # STEP 5: 核心统计计算（并行生成 4 种检验结果）

  # 初始化存储容器
  p_list_ttest   <- list()
  p_list_dunnett <- list()
  p_list_dunn    <- list()
  
  for (met in met_order) {
    df_met <- sample_level_top40 %>% filter(Metabolite_Name == met)
    if (!"Syn" %in% df_met$Treatment || length(unique(df_met$Treatment)) <= 1 || nrow(df_met) <= 3) next
    
    syn_flux <- df_met$Community_Net_Flux[df_met$Treatment == "Syn"]
    other_trs <- setdiff(unique(df_met$Treatment), "Syn")
    
    #  计算法 1: Pairwise t-test 
    for (tr in other_trs) {
      tr_flux <- df_met$Community_Net_Flux[df_met$Treatment == tr]
      if (length(syn_flux) >= 2 && length(tr_flux) >= 2) {
        tryCatch({
          p_val <- t.test(tr_flux, syn_flux)$p.value
          p_list_ttest[[length(p_list_ttest) + 1]] <- data.frame(Metabolite_Name=met, Treatment=tr, P_Val=p_val)
        }, error = function(e) {})
      }
    }
    
    #  计算法 3: Dunnett's Test (参数多重比较) 
    tryCatch({
      df_met$Treatment <- factor(df_met$Treatment)
      df_met$Treatment <- relevel(df_met$Treatment, ref = "Syn")
      dunnett_fit <- DescTools::DunnettTest(Community_Net_Flux ~ Treatment, data = df_met, control = "Syn")
      res_matrix <- dunnett_fit[["Syn"]]
      p_list_dunnett[[met]] <- as.data.frame(res_matrix) %>% rownames_to_column("Comparison") %>%
        mutate(Treatment = sub("-Syn", "", Comparison), P_Val = `pval`, Metabolite_Name = met) %>%
        select(Metabolite_Name, Treatment, P_Val)
    }, error = function(e) {})
    
    # 计算法 4: Dunn's Test (非参数多重比较) 
    tryCatch({
      dunn_fit <- FSA::dunnTest(Community_Net_Flux ~ Treatment, data = df_met, method = "none")
      p_list_dunn[[met]] <- dunn_fit$res %>% filter(grepl("Syn", Comparison)) %>%
        mutate(Treatment = case_when(grepl(" - Syn", Comparison) ~ sub(" - Syn", "", Comparison),
                                     grepl("Syn - ", Comparison) ~ sub("Syn - ", "", Comparison), TRUE ~ NA_character_),
               P_Val = P_unadj, Metabolite_Name = met) %>%
        filter(!is.na(Treatment)) %>% select(Metabolite_Name, Treatment, P_Val)
    }, error = function(e) {})
  }
  
  #  统一进行多重比较矫正 (FDR / BH 法) 并转化为星号 
  convert_to_star <- function(df) {
    if(nrow(df) == 0) return(data.frame(Metabolite_Name=character(), Treatment=character(), Letter=character()))
    df %>% mutate(P_Adj = p.adjust(P_Val, method = "BH")) %>%
      mutate(Letter = case_when(P_Adj < 0.01 ~ "**", P_Adj < 0.05 ~ "*", TRUE ~ "")) %>%
      select(Metabolite_Name, Treatment, Letter) %>%
      bind_rows(data.frame(Metabolite_Name = met_order, Treatment = "Syn", Letter = "")) # 补齐Syn组
  }
  
  sig_ttest   <- convert_to_star(bind_rows(p_list_ttest))
  sig_dunnett <- convert_to_star(bind_rows(p_list_dunnett))
  sig_dunn    <- convert_to_star(bind_rows(p_list_dunn))
  
  #  计算法 2: limma 经验贝叶斯检验 
  flux_mat <- sample_level_top40 %>% select(sample_id, Metabolite_Name, Community_Net_Flux) %>%
    pivot_wider(names_from = sample_id, values_from = Community_Net_Flux, values_fill = 0) %>%
    column_to_rownames("Metabolite_Name") %>% as.matrix()
  
  meta_info <- sample_level_top40 %>% select(sample_id, Treatment) %>% distinct() %>% arrange(match(sample_id, colnames(flux_mat)))
  design <- model.matrix(~ 0 + factor(meta_info$Treatment, levels=c("Syn", setdiff(group_names, "Syn"))))
  colnames(design) <- c("Syn", setdiff(group_names, "Syn"))
  
  fit <- lmFit(flux_mat, design)
  cont_matrix <- makeContrasts(contrasts = paste0(setdiff(colnames(design), "Syn"), " - Syn"), levels = design)
  fit2 <- eBayes(contrasts.fit(fit, cont_matrix))
  
  p_list_limma <- list()
  for (i in 1:ncol(cont_matrix)) {
    tr_name <- setdiff(colnames(design), "Syn")[i]
    p_list_limma[[i]] <- topTable(fit2, coef = i, number = Inf) %>% rownames_to_column("Metabolite_Name") %>%
      mutate(Treatment = tr_name, Letter = case_when(adj.P.Val < 0.01 ~ "**", adj.P.Val < 0.05 ~ "*", TRUE ~ "")) %>%
      select(Metabolite_Name, Treatment, Letter)
  }
  sig_limma <- bind_rows(p_list_limma) %>% bind_rows(data.frame(Metabolite_Name = met_order, Treatment = "Syn", Letter = ""))
  
  
  # -
  # STEP 6~9: 自动化循环绘图与双格式（PDF + PNG）导出
  # -
  # 将所有方法打包为配置列表
  methods_config <- list(
    "t_test"      = list(df = sig_ttest,   label = "Flux (Mean ± SE, t-test * vs Syn)",     color = "red"),
    "limma"       = list(df = sig_limma,   label = "Flux (Mean ± SE, limma * vs Syn)",      color = "purple"),
    "dunnett"     = list(df = sig_dunnett, label = "Flux (Mean ± SE, Dunnett * vs Syn)",    color = "blue"),
    "dunn"        = list(df = sig_dunn,    label = "Flux (Mean ± SE, Dunn's test * vs Syn)", color = "darkgreen"),
    "descriptive" = list(df = NULL,        label = "Flux (Mean ± SE, Descriptive Display)", color = "transparent")
  )
  
  # 计算全局绘图常数
  max_flux_val <- max(abs(sink_source_top40$Mean_Flux), na.rm = TRUE)   
  pad_val <- max_flux_val * 0.15      
  dyn_height <- max(4, length(met_order) * 0.2)   
  
  sample_level_plot <- sample_level_top40 %>%
    mutate(Treatment = factor(Treatment, levels = group_names), Metabolite_Name = factor(Metabolite_Name, levels = met_order))
  
  # 开始循环渲染与导出
  for (method_name in names(methods_config)) {
    cfg <- methods_config[[method_name]]
    
    # 融合统计标记
    if (is.null(cfg$df)) {
      plot_df <- sink_source_top40 %>% mutate(Letter = "")
    } else {
      plot_df <- sink_source_top40 %>% left_join(cfg$df, by = c("Treatment", "Metabolite_Name"))
    }
    
    sink_source_final <- plot_df %>%
      complete(Treatment = factor(group_names, levels = group_names), Metabolite_Name, fill = list(Mean_Flux=0, SD_Flux=0, SE_Flux=0, Letter="")) %>%
      mutate(
        Letter = replace_na(Letter, ""),
        Role = if_else(Mean_Flux >= 0, "Producer (Secretion)", "Consumer (Uptake)"),  
        Metabolite_Name = factor(Metabolite_Name, levels = met_order),   
        Label_Y = if_else(Mean_Flux >= 0, Mean_Flux + SE_Flux + pad_val, Mean_Flux - SE_Flux - pad_val)
      ) %>%
      mutate(Treatment = factor(Treatment, levels = group_names))
    
    # 构图
    p <- ggplot(sink_source_final, aes(x = Metabolite_Name, y = Mean_Flux)) +
      geom_col(aes(fill = Role), color = "black", linewidth = 0.2, alpha = 0.6, width = 0.75) +   
      geom_jitter(data = sample_level_plot, aes(x = Metabolite_Name, y = Community_Net_Flux), 
                  color = "grey30", size = 1.0, alpha = 0.5, width = 0.15, height = 0) +
      geom_errorbar(aes(ymin = Mean_Flux - SE_Flux, ymax = Mean_Flux + SE_Flux), 
                    width = 0.3, linewidth = 0.4, color = "black", alpha = 0.8) + 
      geom_text(aes(y = Label_Y, label = Letter), size = 4.0, fontface = "plain", color = cfg$color) +                
      scale_fill_manual(values = c("Producer (Secretion)" = "#D53E4F", "Consumer (Uptake)" = "#3288BD")) +          
      coord_flip() + 
      geom_hline(yintercept = 0, color = "grey30", linewidth = 0.5) +              
      facet_wrap(~ Treatment, nrow = 1, drop = FALSE) + 
      my_theme +                                                                                                     
      scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +                
      labs(title = paste0("Source-Sink Landscape (Method: ", method_name, ")"), 
           subtitle = paste("Sub-Network:", current_macro), 
           x = "", # 因为翻转了，这里代表视觉上的 Y 轴标题（为空）
           y = cfg$label) + # 这里代表视觉上的 X 轴标题
      theme(
        legend.position = "bottom", 
        legend.title = element_blank(), 
        strip.text = element_text(size = 11, face = "bold", family = "sans"),
        
        #  新增/优化：坐标轴字体与大小设置 
        
        # 1. 坐标轴标题 (Axis Titles)
        # axis.title.x 控制视觉上的水平轴标题 (cfg$label)
        axis.title.x = element_text(family = "sans", size = 12, face = "plain", color = "black", margin = margin(t = 10)),
        # axis.title.y 控制视觉上的垂直轴标题 (由于上面 labs 设为了 x="", 这里其实是不显示的，但规范写上以防后续修改)
        axis.title.y = element_text(family = "sans", size = 12, face = "plain", color = "black", margin = margin(r = 10)),
        
        # 2. 坐标轴刻度标签 (Axis Texts)
        # axis.text.x 控制水平轴的数值标签 (Mean_Flux 的数值)
        axis.text.x = element_text(family = "sans", size = 11, face = "plain", color = "black", angle = 30, hjust = 1, vjust = 1),
        # axis.text.y 控制垂直轴的标签 (Metabolite_Name 代谢物名称)。建议加粗，提升可读性。
        axis.text.y  = element_text(family = "sans", size = 11, face = "plain", color = "black")
        
    
      )
    
    # 导出两种格式文件
    ggsave(paste0(dir_macro, "2_SourceSink_", method_name, ".pdf"), p, width = 8, height = dyn_height, bg = "white")
    ggsave(paste0(dir_macro, "2_SourceSink_", method_name, ".png"), p, width = 8, height = dyn_height, dpi = 300, bg = "white")
  }
  
  
  
  
  
  
  

  # 子任务 (4)：单分网络拓扑重构（单环布局 + 网络拓扑统计量比较）

  # - 1. 创建输出目录 -
  # 设定投影后的单部网络（Unipartite Cross-feeding Network）独立输出路径
  dir_uni <- paste0(dir_macro, "4_Uni_Net/")   
  if (!dir.exists(dir_uni)) dir.create(dir_uni)  
  
 
  total_secretion <- macro_dyn_data %>% 
    filter(Net_Flux > 0) %>%                        
    group_by(Treatment, Metabolite_Name) %>%        # 跨处理组、按代谢物分子进行就地归类
    summarise(Sum_Prod = sum(Net_Flux), .groups = "drop")  
  
  # 聚合全群落中所有反向通量（吸收行为），计算该代谢物在特定处理组环境下的“总消耗盘体量”
  total_uptake <- macro_dyn_data %>% 
    filter(Net_Flux < 0) %>%                        # 严格筛选微生物从胞外摄取/利用的负通量
    group_by(Treatment, Metabolite_Name) %>% 
    summarise(Sum_Cons = sum(abs(Net_Flux)), .groups = "drop")  # 绝对值化，统一转化为正向消耗流
  
  cat("      >> [拓扑重构] 正在执行份额分配逻辑：基于供需质量守恒计算有向边权重 (Prod_Share x Cons_Share)...\n")
  
  # - 3. 提取每个生产者（MAG）的分泌信息及份额 -
  # 【供应侧市场份额拆解】：量化单个菌株（MAG）在根际代谢混池某种资源投入中的贡献比例
  producers <- macro_dyn_data %>% 
    filter(Net_Flux > 0) %>%                        # 提取所有具备资源分泌功能的 MAG
    left_join(total_secretion, by = c("Treatment", "Metabolite_Name")) %>%  # 关联该分子的大盘总产量
    mutate(Prod_Share = Net_Flux / Sum_Prod) %>%   # 计算该 MAG 分泌量占大盘总产量的百分比（即供应权重）
    select(
      Group = Treatment,          
      Producer = MAG_ID,
      Metabolite = Metabolite_Name,
      Prod_Flux = Net_Flux,       
      Prod_Share                  
    )
  

  consumers <- macro_dyn_data %>% 
    filter(Net_Flux < 0) %>%                        # 提取所有具备资源吸收功能的 MAG
    mutate(Cons_Flux = abs(Net_Flux)) %>%           # 通量绝对值化，为份额计算做准备
    left_join(total_uptake, by = c("Treatment", "Metabolite_Name")) %>%  # 关联该分子的大盘总消耗量
    mutate(Cons_Share = Cons_Flux / Sum_Cons) %>%   # 计算该 MAG 吸收量占大盘总消耗的百分比（即需求权重）
    select(
      Group = Treatment,
      Consumer = MAG_ID,
      Metabolite = Metabolite_Name,
      Cons_Flux,                
      Cons_Share                
    )

  uni_edges_all <- inner_join(
    producers, 
    consumers, 
    by = c("Group", "Metabolite"),                # 跨处理组，在相同的化学货币（Metabolite）池中进行对偶碰撞
    relationship = "many-to-many"                 # 激活笛卡尔积，全面枚举所有“供-需”物种偶对
  ) %>%   
    filter(Producer != Consumer) %>%                # 【生物学防爆】：严格剥离菌株自我分泌自我吸收的无效内循环
    left_join(total_secretion, by = c("Group" = "Treatment", "Metabolite" = "Metabolite_Name")) %>%
    left_join(total_uptake, by = c("Group" = "Treatment", "Metabolite" = "Metabolite_Name")) %>%
    mutate(
      # 【短板效应约束】：依据 Liebig 最小限制法则，生态系统实际发生流转的资产天花板取决于“供”与“需”的极小值
      System_Exchange = pmin(Sum_Prod, Sum_Cons),
      # 【概率分摊公式】：最终有向边权重 = 系统真实流转总量 * 生产者出资比例 * 消费者占有比例
      Weight = System_Exchange * Prod_Share * Cons_Share
    ) %>% 
    filter(Weight > 1e-4) %>%                       # 【网络降噪】：卡掉背景极微量代谢波动的 solver 噪声，保留核心骨架边
    rename(from = Producer, to = Consumer) %>%      # 规范化图论列名（from -> 生产者，to -> 消费者）
    select(Group, from, to, Metabolite, Weight)     # 提炼跨组单部有向网络的边表标准格式
  
  

  if (nrow(uni_edges_all) > 0) {   
    
    
    
    # - 1. 多重边压缩与大盘混合控边法 -
    cat("      >> [网络降噪] 执行多重边压缩，并应用大盘混池分位数筛选，自适应剥离求解器噪声...\n")
    # - 6. 多重边空间压缩：菌株对偶间流转总通量聚合与主导货币打捞 -
    # 【多流归一化】：将多重有向网（Multi-digraph）压缩为标准加权有向网（Weighted Digraph）
    uni_edges_compressed <- uni_edges_all %>%
      group_by(Group, from, to) %>%                # 跨处理组，严格锁定特定的“生产者-消费者”菌株对偶
      summarise(
        # 【总通量累加】：将两只菌之间所有共存代谢货币的交换权重进行绝对加和，代表它们互作的总体生态强度
        Total_Weight = sum(Weight), 
        
        # 【主导货币识别】：利用 which.max 索引，精准打捞出两只菌之间流转通量最高的那一种代谢物名称
        # 该分子被定义为维系这对共生关系的核心“主力化学货币（Dominant Currency）”
        Dominant_Metabolite = Metabolite[which.max(Weight)], 
        .groups = "drop"
      )
    
    
    # 将最终的 9 大核心全局拓扑统计表落盘，直接对接下游的推断统计与多维可视化
    fwrite(uni_edges_compressed, paste0(dir_macro, "0_Network_Topology_Global_all-edges.csv"))
    
    
    
    
    

    global_threshold <- quantile(uni_edges_compressed$Total_Weight, 1 - QUANTILE_CUT)
    

    uni_edges_raw <- uni_edges_compressed %>%
      filter(Total_Weight >= global_threshold) %>% # 【强力拦截】：卡掉处于大盘底层的微量流转边，物理剥离求解器线性规划的松弛噪声
      rename(
        Weight = Total_Weight,                     # 将总权重重新对齐为通用标准列名 Weight，作为有向边的物理粗细
        Metabolite = Dominant_Metabolite           # 将主导代谢货币对齐为列名 Metabolite，作为边的核心生物学物理标签
      ) 
    
    cat("      >> [指标计算] 计算节点级与全局级拓扑特征以评估群落代谢周转效率...\n")    
    # - 2. 初始化报告容器 -
    network_keystone_report <- list()      
    network_global_stats <- list()         
    
    
    # - 3. 逐组分析网络拓扑 -
    for (g in group_names) {
      
      # g <- "CK"
      
      # - 1. 动态切片：提取当前处理组的特异性互作边表 -
      # 从已经过降噪和骨架提炼的全局边表中，精准抽离出当前正在遍历的处理组 g 的网络数据
      g_edge_stat <- uni_edges_raw %>% filter(Group == g)
      
      # - 2. 防御性编程：空矩阵拦截与零值初始化机制 -
      # 评估分位数过滤后该组的“残存互作边”数量，防御极端胁迫导致网络完全解体的特殊情况
      if (nrow(g_edge_stat) == 0) {
        # 如果该组无有效边存活，直接在全局全局统计列表中将其全指标归零初始化，防止后续矩阵运算报错
        network_global_stats[[g]] <- data.frame(
          Group = g, Nodes = 0, Edges = 0, Density = 0, 
          Avg_Degree = 0, Modularity = 0, Clustering_Coeff = 0,
          Avg_Path_Length = 0, Diameter = 0, Scale_Free_R2 = 0
        )
        next   # 【核心安全阀】：优雅跳过当前循环，直接进入下一个处理组的分析，绝不中断整条流水线
      }
      
 
      g_obj <- graph_from_data_frame(d = g_edge_stat %>% select(from, to, Weight), directed = TRUE)
      

      dist_weights <- 1 / E(g_obj)$Weight
      
      
      
      
      # 3.1 计算 Keystone 指标 
      

      
      # 【核心枢纽菌株多维拓扑解构】：在当前处理组 g 的有向加权网络中，深挖每个 MAG 的生态位角色
      stat_df <- data.frame(
        # 提取网络中所有节点的唯一标识（对应基因组分析中的标准 MAG_ID 或物种物种名）
        MAG_ID      = V(g_obj)$name,   
        
        # 【出度强度】：代表该菌株对外主动分泌/输出代谢货币的通道多样性（连出的边数）
        Out_Degree  = degree(g_obj, mode = "out"),      
        
        # 【入度强度】：代表该菌株从胞外混池摄取/利用外界资源的环境响应多样性（连入的边数）
        In_Degree   = degree(g_obj, mode = "in"),        
        
        # 【超级供体指数 (Hub Score)】：基于 HITS 算法，并以流转通量 Weight 为权重。
        # 一个 MAG 的 Hub 评分越高，说明它不仅对外输出的通量巨大，而且其下游连接的都是极其关键的“大宗消费者”。
        # 在生物学上，这代表根际微生态中撑起底层资源大盘的“核心超级奶妈株”。
        Hub_Score   = hub_score(g_obj, weights = E(g_obj)$Weight)$vector,
        
        # 【超级受体指数 (Authority Score)】：基于 HITS 算法，并以流转通量 Weight 为权重。
        # 一个 MAG 的 Authority 评分越高，说明它能高效捕获、汇聚并消化那些由核心供体菌排出的关键代谢物。
        # 在生物学上，这代表网络中的“顶级功能放大器”或专性次级降解者。
        Authority   = authority_score(g_obj, weights = E(g_obj)$Weight)$vector,
        
        # 【中枢介数中心度 (Betweenness)】：基于空间路径成本（dist_weights = 1 / Weight）进行严格解析。
        # 衡量一个节点作为“代谢中转桥梁”的控制力。Betweenness 高的菌株扼守着不同代谢模块之间唯一的流转通道，
        # 一旦该菌株因环境胁迫（如干旱）灭绝，整个网络的不同功能群落就会彻底断联、分崩离析。
        Betweenness = betweenness(g_obj, weights = dist_weights, directed = TRUE) 
      ) %>% 
        mutate(
          Group = g,                                    # 物理标记当前处理组标签，防止跨组归档时发生数据混淆
          
          # 【多维联合赋分】：将“核心供体”与“核心受体”实力进行等权重加和，构建全局多功能枢纽复合评分。
          # 提示：该综合得分极高者，属于同时横跨多模块、既分泌又吸收的“全能底盘菌株（Flagship Taxa）”。
          Keystone_Score = Hub_Score + Authority   
        ) %>% 
        # 依据综合 Keystone 得分进行从高到低重排，使真正起机制驱动作用的 Top 菌株直接跃然纸上
        arrange(desc(Keystone_Score))   
      
      # 将当前处理组独立算出的全套核心菌株拓扑画像，安全归档至全局矩阵报表中
      network_keystone_report[[g]] <- stat_df
      
      
      
      
      
      # - 3.2 计算全局拓扑统计量 (循环体内组件) -
      # 【群落模块化解构】：基于随机游走算法（Walktrap）识别网络内部的非重叠群落结构（Modules）
      # 以有向边的流转通量 Weight 作为权重，通量越高的菌株对偶越倾向于被聚类在同一个代谢生态功能模块内
      g_wt <- cluster_walktrap(g_obj, weights = E(g_obj)$Weight)
      
      # 【无尺度特征探针】：调用改良版极大似然估计 (MLE) 与 Kolmogorov-Smirnov (KS) 检验函数
      sf_stats <- calculate_scale_free_r2(g_obj, mode = "all")
      
      # =========
      # 【类型防爆隔离墙】：安全提取标量，强制防御由样本量过小或未收敛导致的长度为 0 或 NULL 报错
      # 完美兼容“早期异常拦截返回（无 xmin，叫 p_value）”与“标准收敛返回（有 xmin，叫 ks_p_value）”
      # =========
      sf_is_scale_free <- if(length(sf_stats$is_scale_free) > 0) sf_stats$is_scale_free else FALSE
      sf_alpha         <- if(length(sf_stats$alpha) > 0) sf_stats$alpha else NA
      sf_xmin          <- if(length(sf_stats$xmin) > 0) sf_stats$xmin else NA
      
      # 兼容两套可能的 P 值命名规范，确保统计学显著性注解永不丢失
      sf_ks_p <- NA
      if(length(sf_stats$ks_p_value) > 0) {
        sf_ks_p <- sf_stats$ks_p_value
      } else if(length(sf_stats$p_value) > 0) {
        sf_ks_p <- sf_stats$p_value
      }
      
      # 【大盘指标矩阵装配】：正式构建当前处理组 g 的全局宏观拓扑行为画像数据框
      network_global_stats[[g]] <- data.frame(
        Group            = g,                                                         # 当前环境处理组标签
        Nodes            = vcount(g_obj),                                             # 网络规模：总参与菌株节点数
        Edges            = ecount(g_obj),                                             # 网络复杂性：总有效交互边数
        Density          = edge_density(g_obj),                                       # 网络紧密度：实际边数占最大可能连线数的比例
        Avg_Degree       = mean(degree(g_obj, mode = "all")),                        # 群落活跃度：平均每个物种拥有的代谢协作通道数
        Modularity       = tryCatch(modularity(g_wt), error = function(e) 0),        # 结构稳健性：模块度评分（tryCatch 防御无群落分化时的计算崩溃）
        Clustering_Coeff = transitivity(g_obj, type = "global"),                      # 协作凝聚力：全局群聚系数（反映网络局部抱团协同的紧密程度）
        Avg_Path_Length  = mean_distance(g_obj, weights = dist_weights, directed = TRUE), # 周转效率：平均路径长度（基于阻力倒数权重，值越小代谢流转越快）
        Diameter         = diameter(g_obj, weights = dist_weights, directed = TRUE),  # 传输极限：网络拓扑直径（群落内部最远的物质传递链物理跨度）
        
        # 填入上述安全防御提取后的无尺度特征核心参数（确保每一列长度严格为 1，杜绝行列对齐崩溃）
        Is_Scale_Free   = sf_is_scale_free,                                          # 现代科学范式下的无尺度特征判别判定 (TRUE/FALSE)
        Power_Law_Alpha = sf_alpha,                                                   # 幂律指数（无尺度特征网络的骨架斜率参数）
        Power_Law_Xmin  = sf_xmin,                                                    # 幂律分布的最小度数截断临界点
        KS_P_Value      = sf_ks_p                                                     # 拟合优度检验 P 值（大于 0.1 则代表成功通过幂律分布假设）
      ) %>% 
        # 【选择性数据清洗】：仅针对数值型拓扑指标中的 NA 填补为 0，物理保护 Group 字符列和布尔列不受破坏
        mutate(across(where(is.numeric), ~ replace_na(.x, 0)))
      
      
      # - 4. 保存 Keystone 与全局统计结果 (循环体外组件) -
      # 【数据流聚合与跨组归档】：当所有处理组的循环迭代全部安全跑通后，在此启动大盘收网
      
      # 1. 纵向物理堆叠所有处理组的节点级中心度数据（Keystone Taxonomy），融合成统一的长表底稿
      final_report <- bind_rows(network_keystone_report)
      # 采用高通量快写引擎将全菌株拓扑实力画像导出为 CSV
      fwrite(final_report, paste0(dir_macro, "Keystone_Stats.csv"))
      
      # 2. 纵向物理堆叠所有处理组的系统级宏观拓扑行为指标，形成处理组间的横向对比矩阵
      global_stats_df <- bind_rows(network_global_stats)
      # 将最终的 9 大核心全局拓扑统计表落盘，直接对接下游的推断统计与多维可视化
      fwrite(global_stats_df, paste0(dir_macro, "Network_Topology_Global_Stats.csv"))
      
      
      
      
    =
      # 在全局数据中计算 Degree 提取 Top 5 核心节点
    =
      global_uni_nodes <- data.frame(name = unique(c(uni_edges_raw$from, uni_edges_raw$to)))
      uni_graph <- graph_from_data_frame(
        d = uni_edges_raw %>% select(from, to) %>% distinct(), 
        vertices = global_uni_nodes, 
        directed = TRUE
      )
      node_degrees <- degree(uni_graph, mode = "all")
      top5_core_nodes <- names(sort(node_degrees, decreasing = TRUE)[1:min(5, length(node_degrees))])
      
      cat("      >> [动态核心] 基于全局网络连通度，已提取 Top 5 节点作为核心层: ", paste(top5_core_nodes, collapse = ", "), "\n")
      
      
      
      
      
      
      
      # - 科学性升级：象限分析与渲染 -
      cat("      >> [拓扑重构] 绘制 Keystone 二维象限图，识别代谢网络中的绝对核心枢纽...\n")
      if (nrow(final_report) > 0) {
        
      
        # 1. 动态阈值解构：计算各处理组内部拓扑分布的黄金分割中位数
      
        # 针对偏态分布的拓扑数据，采用中位数（median）代替均值（mean），能更稳健地抵抗极端Hubs的拉平效应
        # 提示：若后续想筛选极度严苛的头部精英节点，可将 median(x) 替换为 quantile(x, 0.75)
        # 已修改：使用 quantile(x, 0.75) 筛选极度严苛的头部精英节点（前 25%）
        quadrant_cutoffs <- final_report %>%
          group_by(Group) %>%
          summarise(
            cutoff_out = quantile(Out_Degree, 0.75, na.rm = TRUE),
            cutoff_bet = quantile(Betweenness, 0.75, na.rm = TRUE),
            .groups = "drop"
          )
        
      
        # 2. 空间矩阵投影：基于供需广度与中继控制力划分四大生态象限
      
        quadrant_plot_data <- final_report %>%
          left_join(quadrant_cutoffs, by = "Group") %>%
          group_by(Group) %>%
          mutate(
            # 依据双轴阈值碰撞，将 MAG 划分到对应的生态功能格点中
            Quadrant = case_when(
              Out_Degree >= cutoff_out & Betweenness >= cutoff_bet ~ "Q1: Keystone Hubs",
              Out_Degree < cutoff_out  & Betweenness >= cutoff_bet ~ "Q2: Intermediaries",
              Out_Degree < cutoff_out  & Betweenness < cutoff_bet  ~ "Q3: Peripheral",
              Out_Degree >= cutoff_out & Betweenness < cutoff_bet  ~ "Q4: Local Sources"
            ),
            # 判定当前菌株是否属于上游算法锁定的全局 Top 5 绝对核心层 (保留该逻辑仅用于标签白名单)
            Is_Core = MAG_ID %in% top5_core_nodes
          ) %>%
          
        
        # 3. 【核心技术升级】：智能标签筛选漏斗，防御图表文字堆叠
      
        # 步骤A：在每个处理组的每个象限内部，对真正的 Q1 核心枢纽菌按控制力实力实施严格排序
        group_by(Group, Quadrant) %>%
          mutate(
            # 将 row_number 升级为可重复性更强的 min_rank，处理介数并列时的科学排位
            Q1_Rank = if_else(Quadrant == "Q1: Keystone Hubs", min_rank(desc(Betweenness)), NA_integer_)
          ) %>%
          
          # 步骤B：组内解绑，重新切片，执行自适应标签释放
          group_by(Group) %>%
          mutate(
            # 【白名单机制】：只有满足以下两个条件之一的“超级明星菌”才会被赋予 Label 文本：
            # 条件1：它是全局 Top 5 核心（Is_Core == TRUE）
            # 条件2：它成功跻身当前处理组 Q1 象限的前 5 名（Q1_Rank <= 5），从而剔除了靠近中位数交叉线的低金量节点
            Label = ifelse(Is_Core | (Quadrant == "Q1: Keystone Hubs" & Q1_Rank <= 5), as.character(MAG_ID), "")
          ) %>%
          ungroup() %>%   # 【安全防爆】：及时释放全局分组状态，杜绝影响后续的数据流操作
          
        
        # 4. 因子对齐：强制锁定处理组在图表 Facet 渲染时的先后次序
      
        mutate(Group = factor(Group, levels = group_names))
        
        
        
        # 加载必要的包（假设已加载 ggplot2, ggrepel）
        # library(ggplot2)
        # library(ggrepel)
        
        # 主绘图对象：使用 ggplot() 构建基础图层
        p_quadrant <- ggplot(quadrant_plot_data, aes(x = Out_Degree, y = Betweenness)) +
          
          
          
          #  1. 阈值参考线 
             geom_hline(data = quadrant_cutoffs, aes(yintercept = cutoff_bet), linetype = "dashed", color = "grey50") +
          geom_vline(data = quadrant_cutoffs, aes(xintercept = cutoff_out), linetype = "dashed", color = "grey50") +
          
          # - 2. 核心散点图层 -
               geom_point(aes(fill = Quadrant, size = Keystone_Score), shape = 21, color = "black", alpha = 0.8) +
          
                 scale_fill_manual(values = c(
            "Q1: Keystone Hubs"  = "#D53E4F",
            "Q2: Intermediaries" = "#FDAE61",
            "Q3: Peripheral"     = "#E0E0E0",
            "Q4: Local Sources"  = "#3288BD"
          )) +
          # 控制最小点和最大点的绝对尺寸范围（比如从 3 到 8，你可以随意更改这两个数字）
          scale_size_continuous(range = c(0.5, 3.5)) +
              geom_text_repel(aes(label = Label), size = 3, fontface = "bold.italic", family = "sans",
                        box.padding = 0.5, max.overlaps = 50, segment.color = "grey40", min.segment.length = 0.1) +
             facet_wrap(~ Group, scales = "free", nrow = 1, ncol = 4) +
          
            scale_y_continuous(expand = expansion(mult = c(0.05, 0.22))) + 
          scale_x_continuous(expand = expansion(mult = c(0.05, 0.12))) + 
          coord_cartesian(clip = "off") +
           my_theme +
          
        
        labs(
          title = "2D Quadrant Projection of Ecological Keystone Hubs",
          subtitle = paste("Module:", current_macro),   # current_macro 为外部变量，指示当前模块名
          x = "Out-Degree (Resource Supply Breadth)",   # 出度反映资源供给广度
          y = "Betweenness Centrality (Metabolic Routing Control)", # 介数中心性反映代谢途径控制力
          fill = "Ecological Role",                     # 填充图例标题
          size = "Original Keystone Score"              # 大小图例标题
        ) +
          
         
        theme(
          # 图例位置：置于底部，并采用【水平排列】(horizontal)避免1x4分面时纵向空间被过度挤压报错
          legend.position = "bottom",
          legend.box = "horizontal",
          legend.background = element_blank(),          # 移除图例背景
          strip.text = element_text(size = 10, face = "bold", family = "sans"),  # 分面标题样式
          axis.title = element_text(size = 10, face = "bold", family = "sans"),  # 轴标题样式
          axis.text  = element_text(size = 10, family = "sans")                  # 轴刻度文本样式
        ) +
          
   
        guides(
          # 填充图例（象限）：覆盖默认的点形状为圆形（21）并固定大小 3，使其美观一致，设置顺序为 1（最前）。
          fill  = guide_legend(override.aes = list(shape = 21, size = 3), order = 1),
          # 大小图例（Keystone Score）：显示为连续渐变圆点，顺序 2。
          size  = guide_legend(order = 2)
          # 【已删除 shape 图例，不再区分 top5 的专属形状提示】
        )
        
    
        
        # 【画幅防崩修复】：1行4列需要采用“带鱼屏”的宽比例输出（16x5.5），否则1x4挤在3x5的画幅里会直接触发零维度崩溃
        ggsave(paste0(dir_macro, "4.5_Keystone_Quadrant-2.pdf"), p_quadrant, width = 10, height = 3.2, bg = "white")
        ggsave(paste0(dir_macro, "4.5_Keystone_Quadrant-2.png"), p_quadrant, width = 10, height = 3.2, dpi = 300, bg = "white")
      }
      
      
      
      
      # - 5. 提取 Top5 Keystone 菌株的代谢细节 -
      top_keystones <- final_report %>% 
        group_by(Group) %>% 
        slice_max(order_by = Keystone_Score, n = 5, with_ties = FALSE) %>% 
        pull(MAG_ID) %>% 
        unique()
      
      keystone_feeding_details <- macro_dyn_data %>% 
        filter(MAG_ID %in% top_keystones) %>% 
        mutate(Direction = ifelse(Net_Flux > 0, "Secretion (Export)", "Uptake (Import)")) %>%
        select(Treatment, MAG_ID, Metabolite_Name, MacroCategory, Direction, Net_Flux, Gross_Intensity) %>%
        arrange(Treatment, MAG_ID, desc(Net_Flux))
      
      fwrite(keystone_feeding_details, paste0(dir_macro, "Keystone_Feeding.csv"))
      
      
      

      top_mets <- uni_edges_raw %>% 
        group_by(Metabolite) %>% 
        summarise(W = sum(Weight), .groups = "drop") %>%           # 累加每种代谢物在全网的实体流转通量
        slice_max(n = 30, order_by = W, with_ties = FALSE) %>%   # 严格提取前 20 种大宗商品（不引入并列，锁定图例大小）
        pull(Metabolite)                                           # 转化为字符向量，作为后续图形着色的“白名单”
      

      uni_edges_raw <- uni_edges_raw %>% 
        mutate(

          Metabolite_Color = ifelse(Metabolite %in% top_mets, Metabolite, "Other")
        )
      

      uni_comm <- cluster_walktrap(uni_graph, weights = E(uni_graph)$Weight)
      

      global_uni_nodes$Cluster <- as.factor(membership(uni_comm))
      
      cat("      >> [网络布局] 应用单圈视觉布局 (Single Ring Layout)：依据模块聚类环绕...\n")
      

      BASE_RADIUS  <- 5.0     # 基础核心半径天花板（保证低节点数时画布不致过小）
      NODE_SPACING <- 0.15    # 节点间距膨胀系数（每个节点分配的圆弧弧长资产）
      
      # 【拓扑秩序洗牌】：对大盘节点进行功能群重排，这是单圈模块抱团的核心灵魂
      nodes_df <- global_uni_nodes %>% 
        mutate(is_core = name %in% top5_core_nodes) %>%
        # 【核心物理排序】：首要按功能模块（Cluster）排序，次要将模块内部的 Top 5 核心节点置顶。
        # 这样在极坐标展开时，同模块物种在圆周上被死死物理相邻，彻底梳理乱线，凸显区室化特征。
        arrange(Cluster, desc(is_core)) 
      
      n_nodes <- nrow(nodes_df) # 动态盘点当前单部网络实际参与流转的节点总数
      
      # 【自适应画幅控制】：依据物种丰富度动态发散圆环半径，物种越多圆环越大，完美预留排斥标签空间
      dynamic_radius <- max(BASE_RADIUS, n_nodes * NODE_SPACING)
      
      # 极坐标三角函数投影矩阵构建（防御 0 节点、1 节点的极端崩溃边界）
      if (n_nodes == 0) {
        custom_layout_dict <- data.frame(name = character(0), x = numeric(0), y = numeric(0))
      } else if (n_nodes == 1) {
        custom_layout_dict <- data.frame(name = nodes_df$name, x = 0, y = 0)
      } else {
        # 在 [0, 2π] (360度) 几何圆周上进行等间距辐射状切片分流
        angles <- seq(0, 2 * pi, length.out = n_nodes + 1)[1:n_nodes]
        
        # 运用经典欧几里得几何投影公式：X = r * cos(theta), Y = r * sin(theta) 将一维长表无损映射为二维圆环坐标
        custom_layout_dict <- data.frame(
          name = nodes_df$name,
          x = dynamic_radius * cos(angles),
          y = dynamic_radius * sin(angles)
        )
      }
      

      library(ggsci)

      met_colors <- c(ggsci::pal_igv("default")(length(top_mets)), "#BFEEFC")
      names(met_colors) <- c(top_mets, "Other") 
      all_met_levels <- names(met_colors)
      
      # 9b. 节点填充色（Node Fill）：动态捕捉全网演化出的群落模块总数（num_clusters）
      num_clusters <- length(unique(global_uni_nodes$Cluster))
      # 利用 colorRampPalette 对经典 NPG 期刊色谱进行自适应平滑内插，
      # 确保无论产生多少个代谢功能模块（Cluster），每个模块都能被赋予独立、优雅且印刷色盲友好的学术主色调
      cluster_colors <- setNames(
        colorRampPalette(ggsci::pal_npg()(min(10, num_clusters)))(num_clusters), 
        levels(global_uni_nodes$Cluster)
      )
      

      max_w_uni <- max(uni_edges_raw$Weight, na.rm = TRUE)
      
      # 提取全基因组底盘层面的绝对最大适应度（Fitness）极值
      global_max_fitness <- max(mag_stats$Fitness, na.rm = TRUE)

      
      
      
      
      #  10. 按处理组逐一绘图 
     
      uni_plots <- list()
      # 标记是否为第一个组（用于控制图例仅显示在第一个子图中，避免冗余）
      is_first_group <- TRUE 
      
      # 遍历每个处理组
      for (g in group_names) {
        
        #  1. 提取该组的边数据（代谢物交换） 
        g_edges <- uni_edges_raw %>% 
          filter(Group == g) %>%   
          select(from, to, Metabolite_Color, Weight, Metabolite) %>% 
          mutate(
            # 若代谢物类型为“Other”，则边透明度降低（淡化次要代谢物）
            edge_alpha = ifelse(Metabolite_Color == "Other", 1, 1)
          )
        
        #  2. 提取该组的节点适应度数据 
        g_fitness <- mag_stats %>% 
          filter(Group == g) %>% 
          select(compartment, Fitness)
        
        #  3. 组装节点属性（坐标、适应度、模块、核心标记等） 
        # 【新增】：提前从四大生态象限计算结果中提取当前组的象限分类信息
        current_quadrant <- quadrant_plot_data %>% 
          filter(Group == g) %>% 
          select(MAG_ID, Quadrant)
        
        g_vertices <- custom_layout_dict %>%   
          left_join(global_uni_nodes, by = "name") %>%   # 合并节点基本信息（Cluster等）
          left_join(g_fitness, by = c("name" = "compartment")) %>% # 合并适应度
          left_join(current_quadrant, by = c("name" = "MAG_ID")) %>% # 【新增】：合并四大象限属性
          mutate(
            Quadrant = replace_na(Quadrant, "Neighbor Nodes"), # 填补可能无象限的边缘节点
            Fitness = replace_na(Fitness, 1e-5),  # 缺失适应度设为极小值（避免零）
            is_core = name %in% top5_core_nodes,  # 标记是否为Top5核心节点
            # 节点透明度：核心=1；非核心且适应度极低（<1e-4）则半透明（0.5），否则0.9
            v_alpha = ifelse(is_core, 1, ifelse(Fitness < 1e-4, 1, 1)), 
            v_stroke = ifelse(is_core, 0, 0),           # 核心节点边框更粗
            v_border = ifelse(is_core, "#EE6A74", "#02C5FD"), # 核心边框红色，普通蓝色
            # 【优化 2】：智能过滤标签，仅显示 key_bin 与 "Q1: Keystone Hubs" 标签
            Label = ifelse(name %in% key_bin | Quadrant == "Q1: Keystone Hubs", as.character(name), "")
          )
        
        #  4. 过滤掉孤立节点（没有边的节点） 
        active_nodes <- unique(c(g_edges$from, g_edges$to))
        g_vertices <- g_vertices %>% filter(name %in% active_nodes)
        
        #  5. 如果存在有效边，则构建并保存网络 
        if (nrow(g_edges) > 0) {
          # 5a. 输出边和节点表（CSV），供外部查看或后续分析
          fwrite(g_edges %>% rename(Source = from, Target = to, Label = Metabolite), 
                 paste0(dir_uni, g, "_Edges.csv"))
          fwrite(g_vertices %>% rename(Id = name) %>% mutate(Label = Id), 
                 paste0(dir_uni, g, "_Nodes.csv"))
          
          # 5b. 创建 igraph 图对象（有向）
          g_graph <- graph_from_data_frame(d = g_edges, vertices = g_vertices, directed = TRUE)
          
          # 5c. 使用预定义的布局坐标（manual layout）
          g_lay <- create_layout(
            g_graph, 
            layout = "manual", 
            x = g_vertices$x[match(V(g_graph)$name, g_vertices$name)], 
            y = g_vertices$y[match(V(g_graph)$name, g_vertices$name)]
          )
          
          # 5d. 使用 ggraph 绘制网络
          p_uni <- ggraph(g_lay) +   
            # - 绘制边（有向弧线） -
          geom_edge_fan(
            aes(
              edge_color = factor(Metabolite_Color, levels = all_met_levels), # 按代谢物类型着色
              edge_width = Weight,      # 边宽表示通量大小
              edge_alpha = edge_alpha   # 透明度区分“Other”和其他
            ),
            arrow = arrow(length = unit(2.0, 'mm'), type = "closed"),  # 箭头表示方向
            start_cap = circle(1.5, 'mm'),  # 起始端留白，避免覆盖节点
            end_cap = circle(3.5, 'mm')     # 终点留白，使箭头清晰
          ) +
            # 设置边透明度映射（使用 identity，因为已直接赋值）
            scale_edge_alpha_identity(guide = "none") +
            # 边颜色映射（使用预定义调色板，确保所有代谢物类型均有颜色）
            scale_edge_color_manual(
              values = met_colors,            
              name = "Metabolite Exchanged", 
              drop = FALSE,                  
              limits = all_met_levels        
            ) +
            # 边宽度连续映射（缩放范围 0.15~2.0，上限由 max_w_uni 决定）
            scale_edge_width_continuous(
              range = c(0.15, 1.5),            
              limits = c(0, max_w_uni),        
              name = "Flux Weight"   
            ) +
            
            
            # - 绘制节点（点） -
          # - 绘制节点（点） -
          geom_node_point(
            aes(
              size = Fitness,          # 节点大小表示适应度
              alpha = v_alpha,         # 透明度（核心/适应度高低决定）
              fill = Quadrant,         # 【优化 1】：填充色改为依据四大生态象限映射
              color = v_border,        # 边框色（手动指定，核心红色，普通蓝色）
              stroke = v_stroke        # 边框粗细
            ),
            shape = 21  # 圆形带边框
          ) +
            # 【优化 1】：节点填充色映射为四象限专属颜色方案 (高对比度学术色板)
            scale_fill_manual(
              values = c(
                "Q1: Keystone Hubs"  = "#D53E4F",
                "Q2: Intermediaries" = "#FDAE61",
                "Q3: Peripheral"     = "#E0E0E0",
                "Q4: Local Sources"  = "#3288BD",
                "Neighbor Nodes"     = "#C4C2C4"
              ),
              name = "Ecological Quadrant"
            ) +
            # 节点大小连续映射（适应度缩放）
            scale_size_continuous(        
              name = "MAG Fitness",
              range = c(2, 7),
              limits = c(0, global_max_fitness)
            ) +
            # 边框色直接使用 identity（已在数据中指定），不生成图例
            scale_color_identity(guide = "none") +   
            scale_alpha_identity(guide = "none") +   
            # - 节点标签（文字） -
          geom_node_text(
            aes(
              # 【优化 2】：调用 Label 属性，结合 NA_character_ 完美隐藏多余的重叠标签
              label = ifelse(Label == "", NA_character_, Label), 
              # 标签文字大小：若节点属于 Top5 核心节点，设为 3.5；否则 2.5。
              size = I(ifelse(name %in% top5_core_nodes, 3.5, 3.5)) 
            ),
            repel = TRUE,            # 启用文字排斥算法，自动调整标签位置以避免相互重叠
            fontface = "bold.italic", # 字体样式：粗体 + 斜体
            box.padding = 0.1,       
            point.padding = 0.1,     
            bg.color = NA,      
            bg.r = 0.00,             
            max.overlaps = Inf,      
            show.legend = FALSE      
          ) +
            
            
            
            # 标题和主题
            labs(title = paste0(g, " Sub-Network")) +   
            theme_void() +   # 清空背景、网格等
            theme(
              plot.title = element_text(size = 12, face = "bold", hjust = 0.5, margin = margin(b = 5)),
              plot.background = element_rect(fill = "white", color = NA),  
              # 仅第一个组显示图例，其余隐藏（避免重复）
              legend.position = if(is_first_group) "bottom" else "none" 
            )
          
          # 5e. 保存图形（PDF 和 PNG）
          ggsave(paste0(dir_uni, g, "_Uni.pdf"), p_uni, width = 8, height = 7, bg = "white")
          ggsave(paste0(dir_uni, g, "_Uni.png"), p_uni, width = 8, height = 7, dpi = 300, bg = "white")
          # 存储绘图对象
          uni_plots[[g]] <- p_uni
          
          # 更新标记：第一个组已处理，后续组不显示图例
          is_first_group <- FALSE 
          
        } else {
          #  6. 如果没有有效边（网络为空），生成占位图提示无连接 
          p_empty <- ggplot() + 
            theme_void() + 
            labs(title = paste(g, " Sub-Network\n(No Active Connections)")) +
            theme(plot.title = element_text(hjust = 0.5, face = "italic", size = 13), 
                  plot.background = element_rect(fill = "white", color = NA))
          # 同样保存为PDF和PNG
          ggsave(paste0(dir_uni, g, "_Uni.pdf"), p_empty, width = 8, height = 7, bg = "white")
          ggsave(paste0(dir_uni, g, "_Uni.png"), p_empty, width = 8, height = 7, dpi = 300, bg = "white")
          # 存储空图对象
          uni_plots[[g]] <- p_empty
        }
      }    
      # - 11. 拼合所有子图 -
      library(patchwork)
      
      cat("      >> [拼图构建] 正在清洗底色、强制注入标题并统一图例尺度...\n")
      
      # 1. 强制为每个子图注入纯白背景和独立标题，切断导致背景发灰的源头
      p_syn  <- uni_plots[["Syn"]]    + ggtitle("Syn Sub-Network")    + theme(panel.background = element_rect(fill = "white", color = NA))
      p_ck   <- uni_plots[["CK"]]     + ggtitle("CK Sub-Network")     + theme(panel.background = element_rect(fill = "white", color = NA))
      p_spse <- uni_plots[["S.Pse."]] + ggtitle("S.Pse. Sub-Network") + theme(panel.background = element_rect(fill = "white", color = NA))
      p_ssph <- uni_plots[["S.Sph."]] + ggtitle("S.Sph. Sub-Network") + theme(panel.background = element_rect(fill = "white", color = NA))
      
      # 2. 重新执行 2x2 拼合逻辑
      p_uni_comp <- (p_syn + p_ck) / (p_spse + p_ssph) +
        
        # 收集全局图例
        plot_layout(guides = "collect") + 
        
        # 全局大标题与整体画框设置
        plot_annotation(
          title = paste("Global Community MAG Cross-Feeding -", current_macro), 
          theme = theme(
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 20)),
            plot.background = element_rect(fill = "white", color = NA),
            plot.margin = margin(t = 15, r = 15, b = 15, l = 15) 
          )
        ) & 
        
        # 使用 '&' 穿透所有子图进行深度主题覆写
        theme(
          # 强制显示并居中子图的标题
          plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
          
          # 彻底清除底层网格和边框
          panel.border = element_blank(),
          panel.grid = element_blank(),
          
          # 图例底色与位置的统一强控
          legend.position = "bottom",
          legend.box = "vertical",
          legend.background = element_rect(fill = "white", color = NA),
          legend.key = element_rect(fill = "white", color = NA),
          legend.title = element_text(size = 11, face = "bold", hjust = 0.5),
          legend.text = element_text(size = 9),
          legend.margin = margin(t = 5, b = 5)
        ) &
        
        # 统一复写 Guides 以修复合并失败导致的重复堆叠
        guides(
          edge_color = guide_legend(title.position = "top", title.hjust = 0.5, ncol = 6, byrow = TRUE, order = 1, override.aes = list(edge_width = 1.5, edge_alpha = 1)),
          edge_width = guide_legend(title.position = "top", title.hjust = 0.5, order = 2),
          fill       = guide_legend(title.position = "top", title.hjust = 0.5, ncol = 6, order = 3, override.aes = list(shape = 21, size = 4, alpha = 1, color = "black", stroke = 0.5)),
          size       = guide_legend(title.position = "top", title.hjust = 0.5, order = 4, override.aes = list(shape = 21, fill = "gray70", color = "black", stroke = 0.5))
        )
      
      # 3. 尺寸与排版优化 (由于图例放到了底部，原高度 16.5 会导致网络图被拉长，这里调整为比例更协调的 10x12)
      ggsave(paste0(dir_macro, "4_All_Uni_Global.pdf"), p_uni_comp, width = 7, height = 15, bg = "white")
      ggsave(paste0(dir_macro, "4_All_Uni_Global.png"), p_uni_comp, width = 7, height = 15, dpi = 300, bg = "white")
      
      
      
      
      # - 12. 绘制各处理组拓扑指标比较柱状图 -
      cat("      >> [高级多维可视化] 采用多元非柱状图策略展示 9 大指标（主/附图深度拆分与布局优化）...\n")
      
      if (nrow(global_stats_df) > 0) {
        # 确保加载需要的包
        if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
        library(patchwork)
        
      
        # 1. 全局数据准备、清洗与因子排序
      
        plot_stats_df <- global_stats_df %>%
          pivot_longer(
            # 【核心修复 1】：将 Scale_Free_R2 替换为新的科学指标 Power_Law_Alpha
            cols = c(Nodes, Edges, Density, Avg_Degree, Modularity, Clustering_Coeff, 
                     Avg_Path_Length, Diameter, Power_Law_Alpha), 
            names_to = "Metric", 
            values_to = "Value"
          ) %>%
          mutate(
            Group = factor(Group, levels = group_names),
            # 【核心修复 2】：因子水平中同步更新为 Power_Law_Alpha
            Metric = factor(Metric, levels = c("Modularity", "Avg_Degree", "Nodes", "Edges", "Density", 
                                               "Clustering_Coeff", "Avg_Path_Length", "Diameter", "Power_Law_Alpha"))
          )
        
        # 明确划分主图（核心机制）与附图（背景参数）指标向量
        main_metrics <- c("Modularity", "Avg_Degree")
        # 【核心修复 3】：附图指标列表中同步更新
        supp_metrics <- c("Nodes", "Edges", "Density", "Clustering_Coeff", "Avg_Path_Length", "Diameter", "Power_Law_Alpha")
        
        df_main <- plot_stats_df %>% filter(Metric %in% main_metrics)
        df_supp <- plot_stats_df %>% filter(Metric %in% supp_metrics)
        
        # 提前提取并合并 CK 基准线数据，加入防重与判空机制
        ck_baseline <- plot_stats_df %>%
          filter(Group == "CK") %>%
          group_by(Metric) %>%
          summarise(CK_Value = mean(Value, na.rm = TRUE), .groups = "drop") # 避免多行重复导致后续映射异常
        
        plot_stats_df_with_ck <- plot_stats_df %>% left_join(ck_baseline, by = "Metric")
        df_main_scatter <- plot_stats_df_with_ck %>% filter(Metric %in% main_metrics)
        df_supp_scatter <- plot_stats_df_with_ck %>% filter(Metric %in% supp_metrics)
        
        
      
      
        cat("        -> [类型二] 正在绘制趋势折线谱图 (Profile Line)...\n")
        
        # 修复 group 映射规范：离散X轴必须指定 group=1 来连贯所有点
        library(ggplot2)
        library(ggsci)
        
        #  p2_main 优化版 
        p2_main <- ggplot(df_main, aes(x = Group, y = Value)) +
          geom_line(group = 1, color = "grey60", linewidth = 0.8, linetype = "dashed") +
          geom_point(aes(color = Group), size = 2.5) +
          geom_text(aes(label = round(Value, 3)), vjust = -1.2, size = 3.5, fontface = "bold") + 
          facet_wrap(~ Metric, scales = "free_y", nrow = 1) +
          scale_color_npg() + 
          # 【核心修改 1】: 设置 Y 轴上下各延伸 10% 的数据范围，完美兼容 free_y
          scale_y_continuous(expand = expansion(mult = c(0.3, 0.3))) + 
          my_theme +  # 注意：自定义 theme 必须放在自定义刻度之后，基础 theme 之前
          labs(title = "Topological Evolution Profiles", x = "Resistance Gradient", y = "Value") +
          theme(
            legend.position = "none",
            
            # 1. 【新增】主标题字体大小及样式设置
            plot.title = element_text(size = 16, face = "bold", color = "#1A252C", 
                                      hjust = 0.5, margin = margin(b = 12)), # hjust=0.5 表示居中偏右，0表示靠左
            
            # 2. 【新增】分面小标题（Metric名字）字体大小及样式设置
            strip.text = element_text(size = 12, face = "bold", color = "#1A252C"),
            # 顺便可以修饰一下分面框的背景（可选，让它更好看）
            strip.background = element_rect(fill = "grey95", color = "grey80"),
            
            # 【核心修改 2】: 优化轴标题（大小、颜色、加粗、间距）
            axis.title.x = element_text(size = 13, color = "#1A252C", face = "bold", margin = margin(t = 10)),
            axis.title.y = element_text(size = 13, color = "#1A252C", face = "bold", margin = margin(r = 10)),
            # 【核心修改 3】: 优化轴刻度标签（大小、深灰色、加粗）
            axis.text.x  = element_text(size = 11, color = "grey20", face = "bold"),
            axis.text.y  = element_text(size = 10, color = "grey20")
          )
        
        #  p2_supp 优化版 
        p2_supp <- ggplot(df_supp, aes(x = Group, y = Value)) +
          geom_line(group = 1, color = "grey70", linewidth = 0.5) +
          geom_point(aes(color = Group), size = 3) +
          facet_wrap(~ Metric, scales = "free_y", nrow = 2) +
          scale_color_npg() + 
          # 【核心修改 1】: Y 轴上下各延伸 10%
          scale_y_continuous(expand = expansion(mult = c(0.3, 0.3))) + 
          my_theme +
          labs(title = "Supplementary Trend Profiles", x = "Treatment Groups", y = "Value") +
          theme(
            legend.position = "none",
            
            # 1. 【新增】主标题字体大小及样式设置
            plot.title = element_text(size = 16, face = "bold", color = "#1A252C", 
                                      hjust = 0.5, margin = margin(b = 12)), # hjust=0.5 表示居中偏右，0表示靠左
            
            # 2. 【新增】分面小标题（Metric名字）字体大小及样式设置
            strip.text = element_text(size = 12, face = "bold", color = "#1A252C"),
            # 顺便可以修饰一下分面框的背景（可选，让它更好看）
            strip.background = element_rect(fill = "grey95", color = "grey80"),
            
            # 【核心修改 2】: 优化轴标题
            axis.title.x = element_text(size = 13, color = "#1A252C", face = "bold", margin = margin(t = 10)),
            axis.title.y = element_text(size = 13, color = "#1A252C", face = "bold", margin = margin(r = 10)),
            # 【核心修改 3】: 优化轴刻度标签（带45度旋转）
            axis.text.x  = element_text(size = 11, color = "grey20", face = "bold", angle = 45, hjust = 1),
            axis.text.y  = element_text(size = 10, color = "grey20")
          )
        
        ggsave(paste0(dir_macro, "5_Alternative_Line_Main.png"), p2_main, width = 4, height = 2.5, dpi = 300, bg = "white")
        ggsave(paste0(dir_macro, "5_Alternative_Line_Supp.png"), p2_supp, width = 7, height = 4.0, dpi = 300, bg = "white")
        
        
      
        # 类型三：Normalized Fingerprint Heatmap（指纹热图）
      
        
        cat("        -> [类型三] 正在绘制标准化网络指纹热图 (Heatmap)...\n")
        
        # 【核心新增】：定义指标在 Y 轴上从上到下的展现顺序（ggplot自下而上绘制，所以用 rev 倒序）
        # 这样可以确保不分面时，核心指标（Core Drivers）依然高亮置顶，背景指标（Background）在下方排列
        metric_order <- rev(c(
          "Avg_Degree", "Modularity",                          # 核心指标
          "Power_Law_Alpha", "Diameter", "Avg_Path_Length",      # 【核心修复 4】：背景指标更新为 Power_Law_Alpha
          "Clustering_Coeff", "Density", "Edges", "Nodes"
        ))
        
        # 修复除以零的数学风险漏洞
        df_heatmap <- plot_stats_df %>%
          group_by(Metric) %>%
          mutate(
            range_val = max(Value) - min(Value),
            Norm_Value = ifelse(range_val == 0, 0.5, (Value - min(Value)) / range_val)
          ) %>%
          ungroup() %>%
          mutate(
            Type = ifelse(Metric %in% main_metrics, "★ Core Drivers", "Background Metrics"),
            # 通过因子化（Factor），强行锁定网络指标在热图中的上下相对位置
            Metric = factor(Metric, levels = metric_order)
          )
        
        p3_heatmap <- ggplot(df_heatmap, aes(x = Group, y = Metric, fill = Norm_Value)) +
          geom_tile(color = "white", linewidth = 0.5) +
          # 适当加粗热图内部的数值加深印刷对比度
          geom_text(aes(label = round(Value, 2)), color = "black", size = 3.5, fontface = "bold") + 
          
          # 【核心修改】：删除了原先的 facet_grid 图层，使图体融为一体
          
          scale_fill_gradient2(low = "#4DBBD5FF", mid = "#FFFFFF", high = "#E64B35FF", midpoint = 0.5,
                               name = "Relative Intensity\n(0 to 1)") +
          my_theme +
          labs(title = "Rhizosphere Cross-feeding Network Fingerprint",
               x = "Treatment Groups", y = "Topological Metrics") +
          theme(
            axis.text.x = element_text(face = "bold", size = 11, color = "black"),
            axis.text.y = element_text(face = "bold", size = 11, color = "black"),
            plot.title  = element_text(face = "bold", size = 14, hjust = 0.5)
            # 移除了分面特有的 strip.text.y 和 panel.spacing 参数
          )
        
      =
        # 布局优化：9行指标在不分面时完美重叠，高度上调至 4.2 英寸，呈现高大上的学术比例
      =
        ggsave(paste0(dir_macro, "5_Alternative_Fingerprint_Heatmap-2.png"), p3_heatmap, width = 7.5, height = 3, dpi = 300, bg = "white")
        ggsave(paste0(dir_macro, "5_Alternative_Fingerprint_Heatmap-2.pdf"), p3_heatmap, width = 7.5, height = 3, bg = "white")
        
        
      
        # 类型四：Matrix Dot Plot（矩阵引导散点图）- 终极 2x2 主图优化版
      
        cat("        -> [类型四] 正在绘制矩阵引导散点图 (Scatter)...\n")
        
        # 
        # 【核心重构】：重新定义主图 4 项与附图 5 项，并强制锁定因子顺序
        # 
        main_metrics <- c("Modularity", "Avg_Degree", "Nodes", "Clustering_Coeff")
        # 【核心修复 5】：散点图附图指标更新
        supp_metrics <- c("Edges", "Density", "Avg_Path_Length", "Diameter", "Power_Law_Alpha") 
        
        df_main_scatter <- plot_stats_df_with_ck %>% 
          filter(Metric %in% main_metrics) %>%
          mutate(Metric = factor(Metric, levels = main_metrics)) # 强锁定 2x2 渲染顺序
        
        df_supp_scatter <- plot_stats_df_with_ck %>% 
          filter(Metric %in% supp_metrics) %>%
          mutate(Metric = factor(Metric, levels = supp_metrics))
        
        # (1) 优化绘图函数：增加 ncol_facet 参数，并扩大 panel.spacing 防止分面挤压
        create_templated_scatter <- function(data, nrow_facet, ncol_facet = NULL) {
          ggplot(data, aes(x = Group, y = Value)) +
            # 即使某些指标没有 CK_Value (NA)，使用 na.rm = TRUE 可以防止画图报错中断
            geom_hline(aes(yintercept = CK_Value), linetype = "dashed", color = "grey60", linewidth = 0.5, na.rm = TRUE) +
            geom_segment(aes(xend = Group, y = CK_Value, yend = Value), color = "grey70", linewidth = 0.4, na.rm = TRUE) +
            geom_point(aes(fill = Group), size = 2.0, shape = 21, color = "black", stroke = 0.6, alpha = 0.95) +
            
            # 引入 ncol 参数以支持强制矩阵排版
            facet_wrap(~ Metric, scales = "free_y", nrow = nrow_facet, ncol = ncol_facet) +
            
            scale_fill_npg() +
            scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
            my_theme +
            labs(x = "Treatment Groups", y = "Index Value") +
            theme(
              legend.position = "none",
              axis.title = element_text(size = 12, face = "bold", family = "sans"),
              axis.text.x = element_text(size = 12, angle = 45, hjust = 1, face = "bold", family = "sans"),
              axis.text.y = element_text(size = 12, face = "bold", family = "sans"),
              strip.text = element_text(size = 12, face = "bold", family = "sans", color = "black"),
              strip.background = element_rect(fill = "grey95", color = NA),
              panel.grid.major.x = element_blank(),
              
              # 针对多列布局，增加分面间的横纵间距，防止坐标轴重叠
              panel.spacing.x = unit(0.5, "lines"),
              panel.spacing.y = unit(0.5, "lines")
            )
        }
        
        # 
        # 主图生成与导出 (2x2 四联屏黄金组合)
        # 
        p4_main <- create_templated_scatter(df_main_scatter, nrow_facet = 2, ncol_facet = 2) +
          labs(title = "Core Rhizosphere Network Drivers (Phenotype-Coupled)") +
          theme(plot.title = element_text(size = 12, face = "bold", family = "sans", hjust = 0.5, margin = margin(b = 15)))
        
        # 输出画幅设为 6.5 x 6，呈现极其方正、干练的 2x2 网格
        ggsave(paste0(dir_macro, "5_Alternative_Scatter_Main_4Metrics.png"), p4_main, width = 5, height = 3.5, dpi = 300, bg = "white")
        ggsave(paste0(dir_macro, "5_Alternative_Scatter_Main_4Metrics.pdf"), p4_main, width = 5, height = 3.5, bg = "white")
        
        
        ggsave(paste0(dir_macro, "5_Alternative_Scatter_Main_4Metrics-2.png"), p4_main, width = 4.8, height = 3.5, dpi = 300, bg = "white")
        ggsave(paste0(dir_macro, "5_Alternative_Scatter_Main_4Metrics-2.pdf"), p4_main, width = 4.8, height = 3.5, bg = "white")
        # 
        # 附图生成与导出 (剩余 5 个指标，采用 2x3 布局)
        # 
        p4_supp <- create_templated_scatter(df_supp_scatter, nrow_facet = 1, ncol_facet = 5) +
          labs(title = "Supplementary Network Topologies") +
          theme(plot.title = element_text(size = 12, face = "bold", family = "sans", hjust = 0.5, margin = margin(b = 10)))
        
        # 2x3 画幅较宽，设为 8.5 x 5.5
        ggsave(paste0(dir_macro, "5_Alternative_Scatter_Supp_5Metrics.png"), p4_supp, width = 8, height = 2.1, dpi = 300, bg = "white")
        ggsave(paste0(dir_macro, "5_Alternative_Scatter_Supp_5Metrics.pdf"), p4_supp, width = 8, height = 2.1, bg = "white")
        
        # 
        # 【全集归档】生成 9分面 (3x3) 组合图备用
        # 
        df_all_scatter <- rbind(df_main_scatter, df_supp_scatter)
        
        p4_combined_9metrics <- create_templated_scatter(df_all_scatter, nrow_facet = 3, ncol_facet = 3) + 
          labs(
            title = "Rhizosphere Cross-feeding Network Topological Fingerprints",
            subtitle = "Dashed line indicates the baseline of Control group (CK)"
          ) +
          theme(
            plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "sans", margin = margin(b = 10)),
            plot.subtitle = element_text(size = 14, italic = TRUE, hjust = 0.5, family = "sans", color = "grey30", margin = margin(b = 15))
          )
        
        # 3x3 必须匹配正方形画幅 (7.5 x 7.5) 保持子图不被压扁
        ggsave(paste0(dir_macro, "5_Alternative_Scatter_9Metrics_Combined.png"), p4_combined_9metrics, width = 7.5, height = 7.5, dpi = 300, bg = "white")
        ggsave(paste0(dir_macro, "5_Alternative_Scatter_9Metrics_Combined.pdf"), p4_combined_9metrics, width = 7.5, height = 7.5, bg = "white")
        
        
      
        # 附加顶级期刊亮点图：2D Adaptation Strategy Space Plot
      
        cat("        -> [机制升华] 正在生成 Modularity vs Avg_Degree 2D 战略空间图...\n")
        
        df_space <- global_stats_df %>% filter(Group %in% group_names)
        
        # 修复向量提取安全漏洞，提取首个匹配值防止多重复导致长度越界
        ck_x  <- if(length(df_space$Avg_Degree[df_space$Group == "CK"]) > 0) df_space$Avg_Degree[df_space$Group == "CK"][1] else NA
        ck_y  <- if(length(df_space$Modularity[df_space$Group == "CK"]) > 0) df_space$Modularity[df_space$Group == "CK"][1] else NA
        syn_x <- if(length(df_space$Avg_Degree[df_space$Group == "Syn"]) > 0) df_space$Avg_Degree[df_space$Group == "Syn"][1] else NA
        syn_y <- if(length(df_space$Modularity[df_space$Group == "Syn"]) > 0) df_space$Modularity[df_space$Group == "Syn"][1] else NA
        
        p5_space <- ggplot(df_space, aes(x = Avg_Degree, y = Modularity)) +
          # 绘制背景参考基准线
          geom_vline(xintercept = ck_x, linetype = "dotted", color = "grey50", na.rm = TRUE) +
          geom_hline(yintercept = ck_y, linetype = "dotted", color = "grey50", na.rm = TRUE) +
          
          # 绘制静态演化箭头
          annotate("segment", x = syn_x, y = syn_y, xend = ck_x, yend = ck_y, 
                   arrow = arrow(length = unit(0.25, "cm"), type = "closed"), 
                   color = "grey65", linewidth = 0.7, na.rm = TRUE) +
          
          # 绘制核心散点
          geom_point(aes(fill = Group), size = 4.5, shape = 21, color = "black", stroke = 0.8) +
          
          # (1) 优化：显式指定文字标签的字体族(family)，使其与全局主题严格一致，微调 vjust 提升美观度
          geom_text(aes(label = Group), vjust = -1.5, fontface = "bold", size = 4.5, family = "sans") +
          
          scale_fill_npg() +
          
          # (2) 优化：使用 expansion 严格令 X、Y 轴上下左右均延伸 15% 的空间
          scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +
          scale_y_continuous(expand = expansion(mult = c(0.15, 0.15))) +
          
          # (3) 优化：关闭画布边缘裁剪，防止顶部的 Group 文字标签被切掉
          coord_cartesian(clip = "off") +
          
          my_theme +
          labs(
            title = "Microbial Network Adaptation Strategy Space",
            subtitle = "Two distinct evolutionary paths converging to high modularity",
            x = "Network Complexity (Avg_Degree)",
            y = "Functional Compartmentalization (Modularity)"
          ) +
          
          # (1) & (4) 优化：全面升级图体文本层级，适配 A4 纸张排版印刷分辨率
          theme(
            legend.position = "none",
            # 标题与副标题样式优化
            plot.title = element_text(size = 16, face = "bold", family = "sans", hjust = 0),
            plot.subtitle = element_text(size = 11, face = "italic", family = "sans", color = "grey30", margin = margin(b = 10)),
            # 坐标轴标签与刻度样式优化
            axis.title.x = element_text(size = 13, face = "bold", family = "sans", margin = margin(t = 8)),
            axis.title.y = element_text(size = 13, face = "bold", family = "sans", margin = margin(r = 8)),
            axis.text.x = element_text(size = 11, face = "bold", family = "sans"),
            axis.text.y = element_text(size = 11, face = "bold", family = "sans")
          )
        
        # (4) 优化：调整图片宽高输出比例（7.5 x 6.2 英寸），完美契合 A4 纸张的半版或单栏学术排版
        ggsave(paste0(dir_macro, "5_Network_Topology_2D_Strategy_Space.png"), p5_space, width = 4, height = 3, dpi = 300, bg = "white")
        ggsave(paste0(dir_macro, "5_Network_Topology_2D_Strategy_Space.pdf"), p5_space, width = 4, height = 3, bg = "white")
      }
      
      
      
    }   # 结束 if (nrow(uni_edges_all) > 0) 条件块
    
    
    
    
    
    gc()
  }
  
  
  
  
  
  
  
  
  
  
  
  ##### 核心代谢物筛选 ####     
  

  # [顶级学术全集收网] 5.0 核心代谢物全局多维特征提取、评分与分面汇总可视化大图

  cat("      >> [大盘功能收网] 正在跨处理组启动全局核心代谢物拓扑-通量双轨评估与分面提炼...\n")
  
  # 1. 全局基础通量与广谱性拓扑统计 (不进行单组 filter，直接按 Group 和 Metabolite 聚合)
  global_metabolite_stats <- uni_edges_raw %>%
    group_by(Group, Metabolite) %>%
    summarise(
      Total_Flux = sum(Weight),                # 维度1：大盘总流通权重
      Interaction_Pairs = n(),                 # 维度2：全网流通过程中的频次广谱性
      Unique_Producers = n_distinct(from),     # 维度3：供给侧源冗余度
      Unique_Consumers = n_distinct(to),       # 维度4：受体侧依赖广泛度
      .groups = "drop"
    )
  
  # 2. 跨组自动化并行解构边介数 (Edge Betweenness)
  global_eb_list <- list()
  for (g_name in group_names) {
    sub_edge_stat <- uni_edges_raw %>% filter(Group == g_name)
    if (nrow(sub_edge_stat) > 0) {
      # 构筑各组有向加权子网对象
      sub_graph <- graph_from_data_frame(d = sub_edge_stat %>% select(from, to, Weight), directed = TRUE)
      # 空间倒数转换：通量越大，拓扑距离越近，周转速率越高
      sub_dist_weights <- 1 / E(sub_graph)$Weight
      sub_eb <- edge_betweenness(sub_graph, directed = TRUE, weights = sub_dist_weights)
      
      sub_edge_stat$Edge_Betweenness <- sub_eb
      
      # 汇聚至组内代谢物级别
      global_eb_list[[g_name]] <- sub_edge_stat %>%
        group_by(Metabolite) %>%
        summarise(Max_Edge_Betweenness = max(Edge_Betweenness, na.rm = TRUE), .groups = "drop") %>%
        mutate(Group = g_name)
    }
  }
  global_edge_betweenness_df <- bind_rows(global_eb_list)
  
  # 3. 安全 Min-Max 缩放防御函数
  safe_scale <- function(x) {
    x[is.na(x)] <- 0  
    rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
    if (rng == 0) return(rep(0.5, length(x))) 
    return((x - min(x, na.rm = TRUE)) / rng)
  }
  
  # 4. 深度拼合多维矩阵，激活组内独立标准化评分系统（完美优化版）
  global_metabolite_report <- global_metabolite_stats %>%
    left_join(global_edge_betweenness_df, by = c("Group", "Metabolite")) %>%
    mutate(Max_Edge_Betweenness = replace_na(Max_Edge_Betweenness, 0)) %>%
    group_by(Group) %>%   # 执行组内独立缩放，防御组间基数差异掩盖
    mutate(
      Score_Flux     = safe_scale(Total_Flux),
      Score_Bridging = safe_scale(Max_Edge_Betweenness),
      
      # 💡【优化方案】：将频次、生产者数、消费者数深度融合，提炼为统一的“生态广谱性得分”
      # 这样既利用了 Unique_Producers/Consumers，又赋予了其“跨物种交流通货”的生态学解释
      Ecological_Breadth = (Interaction_Pairs * 0.4) + (Unique_Producers * 0.3) + (Unique_Consumers * 0.3),
      Score_Breadth  = safe_scale(Ecological_Breadth),
      
      # 🌟 科学权重配比：绝对通量(40%) + 生态广谱度(30%) + 拓扑桥梁阻断力(30%)
      Core_Metabolite_Score = (Score_Flux * 0.4) + (Score_Breadth * 0.3) + (Score_Bridging * 0.3)
    ) %>%
    arrange(Group, desc(Core_Metabolite_Score)) %>%
    ungroup()
  
  # 物理大盘底稿归档导出
  fwrite(global_metabolite_report, paste0(dir_macro, "Global_Core_Metabolites_All_Groups_Stats.csv"))
  
  
  

  # 5. 【核心优化突破点】: 各组提取 Top 15 明星货币并注入分面独立重排机制

 
  
  cat("      >> [景观升华] 正在提取各组 Top 15 取并集，调用 pheatmap 构建带树的全景热图...\n")
  
  # 确保安装生物信息学金标准热图包 pheatmap
  if (!requireNamespace("pheatmap", quietly = TRUE)) install.packages("pheatmap")
  library(pheatmap)
  
  # 1. 严格提取各个处理组内部得分前 15 的代谢物，并跨组取去重并集
  top15_metabolites_per_group <- global_metabolite_report %>%
    group_by(Group) %>%
    slice_max(order_by = Core_Metabolite_Score, n = 15, with_ties = FALSE) %>%
    ungroup()
  
  union_core_metabolites <- unique(as.character(top15_metabolites_per_group$Metabolite))
  cat(paste0("      -> 经计算，四组 Top 15 代谢物去重并集共包含: [", length(union_core_metabolites), "] 种特征分子。\n"))
  
  # 2. 构建严谨的纯数字宽矩阵 (Wide Matrix)，供 pheatmap 引擎底层读取
  wide_data <- global_metabolite_report %>%
    ungroup() %>%
    filter(as.character(Metabolite) %in% union_core_metabolites) %>%
    select(Group, Metabolite, Core_Metabolite_Score) %>%
    distinct() %>%
    pivot_wider(
      names_from = Group, 
      values_from = Core_Metabolite_Score, 
      values_fill = list(Core_Metabolite_Score = 0) # 残缺变异位安全补0
    ) %>%
    as.data.frame()
  
  # 将代谢物名称设为行名，剥离为纯数学矩阵
  rownames(wide_data) <- wide_data$Metabolite
  plot_matrix <- as.matrix(wide_data[, -1])
  
  # 强制锁定顺序为您的抗性梯度实验顺序 (Syn -> CK -> S.Pse. -> S.Sph.)
  plot_matrix <- plot_matrix[, group_names]
  
  # =========
  # 【反转核心操作 1】：将矩阵进行数学转置
  # 转置后：行(Rows) = 处理组，列(Columns) = 代谢物
  # =========
  plot_matrix_t <- t(plot_matrix)
  
  # 3. 制作高级文本标签矩阵 (屏蔽极小值 0.00，保持学术图表清爽感)
  # 注意：这里的行列维度必须按转置后的矩阵 plot_matrix_t 重新对齐
  text_matrix <- matrix(sprintf("%.2f", plot_matrix_t), nrow = nrow(plot_matrix_t))
  text_matrix[plot_matrix_t < 0.005] <- "" # 将0分色块的文本隐形
  
  # 4. 生成 100 阶丝滑学术渐变色 (完美复刻白-橘-红体系)
  color_palette <- colorRampPalette(c( "#F7FBFF",  "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5"))(100)
  
  # =========
  # 【反转核心操作 2】：画幅比例动态翻转
  # 代谢物数量横向排布，动态扩展宽度；处理组纵向排布，高度缩减
  # =========
  dyn_heatmap_width <- max(8, ncol(plot_matrix_t) * 0.25 + 3) # 确保有足够宽度展示聚类树和标签
  dyn_heatmap_height <- max(3.0, nrow(plot_matrix_t) * 0.2 + 2)  # 4个处理组不需要太高
  
  # 6. 渲染带聚类树的热图并直接物理落盘
  pdf_file <- paste0(dir_macro, "5.2_Core_Metabolites_Union_pheatmap2.pdf")
  png_file <- paste0(dir_macro, "5.2_Core_Metabolites_Union_pheatmap2.png")
  
  # 绘制并保存为 PDF
  pheatmap(
    mat = plot_matrix_t,
    color = color_palette,
    
    # 【反转核心操作 3】：聚类与逻辑对调
    cluster_rows = FALSE,                # 关闭 Y 轴聚类（绝不打乱行上的处理组梯度展示顺序）
    cluster_cols = TRUE,                 # 开启 X 轴聚类并在顶部自动绘制树状图（聚合代谢物）
    clustering_distance_cols = "euclidean",
    clustering_method = "ward.D2",       # 采用生态学标准 Ward.D2 强力拉扯同类代谢项
    
    # display_numbers = text_matrix,       # 注入我们过滤后的干净文本矩阵
    # fontsize_number = 9,
    # number_color = "black",
    display_numbers = FALSE,  # text_matrix
    
    fontsize_row = 12,                   # 行上的处理组名可适当放大
    fontsize_col = 10,
    angle_col = 45,                      # 【关键防叠】：代谢物名称在X轴横向铺开，倾斜45度防止文字重叠打架
    
    border_color = "gray60",              # 物理白色网格线隔离色块
    main = "Rhizosphere Pan-Metabolite Functional Profile",
    filename = pdf_file,                 # 直接指定文件路径，底层高速渲染
    width = dyn_heatmap_width,
    height = dyn_heatmap_height
  )
  
  
  
  # 绘制并保存为 PNG
  pheatmap(
    mat = plot_matrix_t,
    color = color_palette,
    cluster_rows = FALSE,
    cluster_cols = TRUE,
    clustering_distance_cols = "euclidean",
    clustering_method = "ward.D2",
    display_numbers = FALSE,  # text_matrix
    # fontsize_number = 9,
    # number_color = "black",
    fontsize_row = 12,
    fontsize_col = 10,
    angle_col = 45,
    border_color = "gray60",
    main = "Rhizosphere Pan-Metabolite Functional Profile",
    filename = png_file,
    width = dyn_heatmap_width,
    height = dyn_heatmap_height
  )
  
  cat("      >> [完美收官] 带灵魂聚类树的顶级全景热图已成功导出为 pheatmap 版本（坐标轴已反转）。\n")
  
  
  
  
  
  
  
  
  
  
  
=====
  # 5. 全局多模块顶级联合合并分析组件 (Cross-Module Meta-Analysis Component)
=====
  cat(">>> [4/6] 正在启动多模块深度交叉合并分析与顶级学术图表提炼...\n")
  
  dir_merge <- paste0(dir_macro, "00_Global_Merged/")
  if (!dir.exists(dir_merge)) dir.create(dir_merge, recursive = TRUE)
  
  core_5_categories <- c("Core Carbon & Energy",
                         "Rhizosphere Exuded Acids & Nutrition",
                         "Drought-Stress Osmoprotectants",
                         "Host Phytohormone Signaling",
                         "Defense Secondary Phenolics")
  
  cat("      >> 正在绘制 5 大生态大盘功能资源重分配堆叠 Profile...\n")
  
  # 【优化 1】：安全聚合分配数据，预防 NA 污染
  global_allocation_data <- flux_final_master %>%
    distinct(sample_id, Treatment, MAG_ID, Metabolite_Name, flux, MacroCategory) %>% 
    filter(MacroCategory %in% core_5_categories) %>%
    filter(flux > 0) %>% # 锁定正向通量（分泌/输出），反映微生物对宿主的资源供给
    group_by(Treatment, MacroCategory) %>%
    summarise(Flux_Sum = sum(abs(flux), na.rm = TRUE), .groups = "drop") %>%
    complete(Treatment = factor(group_names, levels = group_names),
             MacroCategory = factor(core_5_categories, levels = core_5_categories), 
             fill = list(Flux_Sum = 0))
  
  if(nrow(global_allocation_data) > 0) {
    p_alloc <- ggplot(global_allocation_data, aes(x = Treatment, y = Flux_Sum, fill = MacroCategory)) +
      geom_bar(stat = "identity", position = "fill", color = "black", width = 0.55, linewidth = 0.4) +
      scale_y_continuous(labels = scales::percent_format(), expand = c(0,0)) +
      scale_fill_brewer(palette = "Set2", name = "Ecological Modules") +
      my_theme +
      labs(title = "Rhizosphere Metabolic Resource Allocation Shift",
           subtitle = "Trade-off of microbial investment across resistance gradients",
           x = "Treatment Groups", y = "Relative Functional Footprint (%)") +
      theme(legend.position = "right", plot.title = element_text(margin = margin(b=5)))
    
    ggsave(paste0(dir_merge, "1_Func_Alloc.pdf"), p_alloc, width = 9, height = 6, bg="white")
    ggsave(paste0(dir_merge, "1_Func_Alloc.png"), p_alloc, width = 9, height = 6, dpi = 300, bg="white")
  }
  
  cat("      >> 正在跨子网拼装全能枢纽菌株(Keystone Taxonomy)生态蓝图...\n")
  keystone_files <- list.files(path = out_DIR_Main, pattern = "Keystone_Stats.csv", recursive = TRUE, full.names = TRUE)
  
  if(length(keystone_files) > 0) {
    
    # 稳健的文件读取与模块名称清洗
    all_keystones_compiled <- lapply(keystone_files, function(f) {
      dt <- fread(f)
      raw_dir <- basename(dirname(f))
      clean_mod <- gsub("_", " ", raw_dir)
      clean_mod <- gsub("Acids Nutrition", "Acids & Nutrition", clean_mod) 
      clean_mod <- gsub("Carbon Energy", "Carbon & Energy", clean_mod)
      dt[, Module := clean_mod]
      return(dt)
    }) %>% 
      bind_rows() %>%
      filter(Module %in% core_5_categories)
    
    if (nrow(all_keystones_compiled) > 0) {
      fwrite(all_keystones_compiled, paste0(dir_merge, "2_All_Topol_Scores.csv"))
      
      top_global_mags <- all_keystones_compiled %>%
        group_by(Group, MAG_ID) %>%
        summarise(Max_Score_in_Group = sum(Keystone_Score, na.rm = TRUE), .groups = "drop") %>%
        group_by(Group) %>%
        slice_max(order_by = Max_Score_in_Group, n = 8, with_ties = FALSE) %>%
        pull(MAG_ID) %>%
        unique()
      
      # 【优化 2】：严格执行防御性降维与剔除污染 NA
      heatmap_data <- all_keystones_compiled %>%
        filter(MAG_ID %in% top_global_mags) %>%
        mutate(
          Treatment = factor(Group, levels = group_names),
          Module = factor(Module, levels = core_5_categories)
        ) %>%
        filter(!is.na(Treatment)) %>% # 核心防崩拦截
        group_by(Treatment, MAG_ID, Module) %>%
        summarise(Keystone_Score = mean(Keystone_Score, na.rm = TRUE), .groups = "drop") %>%
        complete(Treatment, MAG_ID, Module, fill = list(Keystone_Score = 0))
      
      if (nrow(heatmap_data) > 0) {
        dyn_h_hm <- max(6, length(top_global_mags) * 0.25 + 4)
        p_keystone_map <- ggplot(heatmap_data, aes(x = Module, y = MAG_ID, fill = Keystone_Score)) +
          geom_tile(color = "white", linewidth = 0.3) +
          scale_fill_viridis_c(option = "plasma", name = "Keystone\nScore") +
          facet_wrap(~ Treatment, nrow = 2) + 
          my_theme +
          labs(title = "Multifunctional Mapping of Signature Keystone MAGs",
               subtitle = "Cross-module network importance across contrasting rhizosphere environments",
               x = "", y = "Metagenome-Assembled Genomes (MAGs)") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
                axis.text.y = element_text(face = "bold.italic", size = 10),
                panel.spacing = unit(3, "mm"), legend.position = "right")
        
        ggsave(paste0(dir_merge, "2_Key_MAGs_Heatmap.pdf"), p_keystone_map, width = 13, height = dyn_h_hm, bg="white")
        ggsave(paste0(dir_merge, "2_Key_MAGs_Heatmap.png"), p_keystone_map, width = 13, height = dyn_h_hm, dpi = 300, bg="white")
      }
    } else {
      cat("      !! [警告] 未匹配到任何有效的 core_5_categories 数据，已跳过 Keystone 热图生成。\n")
    }
  }
  
=====
  # 6. 生态关系深度挖掘：引入网络精简、12样本独立解析与 ANOVA 显著性测定
=====
  cat(">>> [5/6] 正在启动高级生态关系挖掘：引入网络精简过滤的推断统计学解析...\n")
  
  dir_eco <- paste0(dir_macro, "00_Ecological_Interactions/")
  if (!dir.exists(dir_eco)) dir.create(dir_eco, recursive = TRUE)
  
  flux_sample_level <- flux_final_master %>%
    distinct(sample_id, Treatment, MAG_ID, Metabolite_Name, .keep_all = TRUE)
  
  sample_total_secretion <- flux_sample_level %>% filter(flux > 0) %>%
    group_by(sample_id, Metabolite_Name) %>% summarise(Sum_Prod = sum(flux, na.rm=TRUE), .groups = "drop")
  
  sample_total_uptake <- flux_sample_level %>% filter(flux < 0) %>%
    group_by(sample_id, Metabolite_Name) %>% summarise(Sum_Cons = sum(abs(flux), na.rm=TRUE), .groups = "drop")
  
  sample_producers <- flux_sample_level %>% filter(flux > 0) %>%
    left_join(sample_total_secretion, by = c("sample_id", "Metabolite_Name")) %>%
    mutate(Prod_Share = flux / Sum_Prod) %>%
    select(sample_id, Treatment, Producer=MAG_ID, Metabolite=Metabolite_Name, Prod_Flux=flux, Prod_Share)
  
  sample_consumers <- flux_sample_level %>% filter(flux < 0) %>%
    left_join(sample_total_uptake, by = c("sample_id", "Metabolite_Name")) %>%
    mutate(Cons_Share = abs(flux) / Sum_Cons) %>%
    select(sample_id, Treatment, Consumer=MAG_ID, Metabolite=Metabolite_Name, Cons_Flux=flux, Cons_Share)
  
  sample_uni_edges_all <- inner_join(sample_producers, sample_consumers, 
                                     by = c("sample_id", "Treatment", "Metabolite"), 
                                     relationship="many-to-many") %>%
    filter(Producer != Consumer) %>%
    left_join(sample_total_secretion, by = c("sample_id", "Metabolite" = "Metabolite_Name")) %>%
    left_join(sample_total_uptake, by = c("sample_id", "Metabolite" = "Metabolite_Name")) %>%
    mutate(
      System_Exchange = pmin(Sum_Prod, Sum_Cons, na.rm=TRUE),     
      Weight = System_Exchange * Prod_Share * Cons_Share 
    ) %>% 
    filter(Weight > 1e-6)
  
  FILTER_METHOD <- "quantile"  
  QUANTILE_CUT <- 0.12         
  TOP_N_EDGES <- 1000          
  
  sample_uni_edges_filtered <- sample_uni_edges_all %>%
    group_by(sample_id) %>%
    { if (FILTER_METHOD == "top_n") {
      slice_max(., order_by = Weight, n = TOP_N_EDGES, with_ties = FALSE)
    } else {
      filter(., Weight >= quantile(Weight, 1 - QUANTILE_CUT, na.rm = TRUE)) # 加入 na.rm 保护
    }
    } %>% ungroup()
  
  # 模块 A：代谢生态位重叠度
  sample_uptake_filtered <- sample_uni_edges_filtered %>%
    mutate(Actual_Uptake = System_Exchange * Cons_Share) %>%
    group_by(sample_id, Treatment, Consumer, Metabolite) %>%
    summarise(Uptake_Flux = sum(Actual_Uptake, na.rm=TRUE), .groups = "drop")
  
  all_samples <- unique(sample_uptake_filtered$sample_id)
  overlap_sample_list <- list()
  
  for(s in all_samples) {
    sample_data <- sample_uptake_filtered %>% filter(sample_id == s)
    tr <- unique(sample_data$Treatment)[1]
    
    mat_data <- sample_data %>%
      select(Consumer, Metabolite, Uptake_Flux) %>%
      pivot_wider(names_from = Metabolite, values_from = Uptake_Flux, values_fill = list(Uptake_Flux = 0)) %>%
      column_to_rownames("Consumer") %>% 
      as.matrix()
    
    if(nrow(mat_data) < 2) next 
    
    # 【优化 3】：矩阵数学防爆。避免分母产生 0 导致全矩阵变为 NaN
    row_norms <- sqrt(rowSums(mat_data^2))
    row_norms[row_norms == 0] <- 1e-9 # 赋予极小值，保护数学除法
    
    sim_mat <- (mat_data %*% t(mat_data)) / (row_norms %*% t(row_norms))
    diag(sim_mat) <- NA
    overlap_sample_list[[s]] <- data.frame(sample_id = s, Treatment = tr, Mean_Competition = mean(sim_mat, na.rm = TRUE))
  }
  overlap_sample_df <- bind_rows(overlap_sample_list)
  
  # 模块 B：交叉喂养强度 
  cf_sample_summary <- sample_uni_edges_filtered %>%
    group_by(sample_id, Treatment) %>%
    summarise(Total_Cooperation_Potential = sum(Weight, na.rm=TRUE), .groups = "drop")
  
  # 模块 C：汇总并依据 Syn 对照组归一化
  eco_replicates <- overlap_sample_df %>%
    left_join(cf_sample_summary, by = c("sample_id", "Treatment")) %>%
    mutate(Treatment = factor(Treatment, levels = group_names))
  
  # 【优化 4】：Syn 基准线安全提取，预防因 Synthetic 组被剔除导致的 NaN 蔓延
  syn_mean_coop <- mean(eco_replicates$Total_Cooperation_Potential[eco_replicates$Treatment == "Syn"], na.rm=TRUE)
  syn_mean_comp <- mean(eco_replicates$Mean_Competition[eco_replicates$Treatment == "Syn"], na.rm=TRUE)
  
  if(is.na(syn_mean_coop) || syn_mean_coop == 0) syn_mean_coop <- 1
  if(is.na(syn_mean_comp) || syn_mean_comp == 0) syn_mean_comp <- 1
  
  eco_replicates <- eco_replicates %>%
    mutate(
      Norm_Coop = Total_Cooperation_Potential / syn_mean_coop,
      Norm_Comp = Mean_Competition / syn_mean_comp,
      CC_Ratio  = Norm_Coop / Norm_Comp
    )
  fwrite(eco_replicates, paste0(dir_eco, "0_Raw_12_Replicates_Eco_Indices.csv"))
  
  # 模块 D：多维推断统计 (含 TryCatch 避免方差为零的统计崩溃)
  run_stats_and_get_letters <- function(data, variable_col, label) {
    # 剔除无法参与 ANOVA 的缺失值
    clean_data <- data %>% filter(!is.na(get(variable_col)))
    if(nrow(clean_data) < 3 || length(unique(clean_data$Treatment)) < 2) {
      return(data.frame(Treatment = unique(data$Treatment), label = "ns", y_pos = 0))
    }
    
    formula_obj <- as.formula(paste(variable_col, "~ Treatment"))
    fit <- aov(formula_obj, data = clean_data)
    
    sink(paste0(dir_eco, "Statistical_Report_", label, ".txt"))
    cat("=== ONE-WAY ANOVA SUMMARY ===\n")
    print(summary(fit))
    cat("\n=== MULTIPLE COMPARISONS (TUKEY HSD: ALL PAIRS) ===\n")
    print(tryCatch(TukeyHSD(fit), error = function(e) "No variance or model failed."))
    sink()
    
    # 自动提取 CLD 字母
    out_hsd <- tryCatch(HSD.test(fit, "Treatment", group = TRUE), error = function(e) NULL)
    
    if(!is.null(out_hsd)) {
      letter_df <- data.frame(Treatment = rownames(out_hsd$groups), label = str_trim(out_hsd$groups$groups))
    } else {
      letter_df <- data.frame(Treatment = unique(clean_data$Treatment), label = "ns")
    }
    
    y_range <- max(clean_data[[variable_col]], na.rm=TRUE) - min(clean_data[[variable_col]], na.rm=TRUE)
    if(y_range == 0) y_range <- 1
    
    anno_df <- clean_data %>%
      group_by(Treatment) %>%
      summarise(max_y = max(get(variable_col), na.rm=TRUE), .groups = "drop") %>%
      left_join(letter_df, by = "Treatment") %>%
      mutate(y_pos = max_y + y_range * 0.08)
    
    return(anno_df)
  }
  
  anno_comp  <- run_stats_and_get_letters(eco_replicates, "Mean_Competition", "1_Competition")
  anno_coop  <- run_stats_and_get_letters(eco_replicates, "Total_Cooperation_Potential", "2_Cooperation")
  anno_ratio <- run_stats_and_get_letters(eco_replicates, "CC_Ratio", "3_CC_Ratio")
  
  # 图 1-3 渲染保持原样 (已完美无误)
  p_overlap <- ggplot(eco_replicates, aes(x = Treatment, y = Mean_Competition)) +
    stat_summary(fun = mean, geom = "bar", aes(fill=Treatment), color = "black", width = 0.55, alpha = 0.85, linewidth = 0.5) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.18, color = "black", linewidth = 0.7) +
    geom_jitter(aes(fill=Treatment), width = 0.15, shape = 21, size = 2.5, color = "black", stroke = 0.5, alpha = 0.8) +
    geom_text(data = anno_comp, aes(x = Treatment, y = y_pos, label = label), size = 6, fontface = "bold", color = "black", vjust=0) +
    scale_fill_brewer(palette = "Set1") +
    my_theme +
    labs(title = "Community Metabolic Niche Overlap (Filtered)",
         subtitle = "Mean ± SE. Different letters indicate significant differences (Tukey's HSD, p < 0.05)",
         x = "Treatment Groups", y = "Cosine Similarity of Uptake Profiles") +
    theme(legend.position = "none")
  
  p_coop <- ggplot(eco_replicates, aes(x = Treatment, y = Total_Cooperation_Potential)) +
    stat_summary(fun = mean, geom = "bar", aes(fill=Treatment), color = "black", width = 0.55, alpha = 0.85, linewidth = 0.5) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.18, color = "black", linewidth = 0.7) +
    geom_jitter(aes(fill=Treatment), width = 0.15, shape = 21, size = 2.5, color = "black", stroke = 0.5, alpha = 0.8) +
    geom_text(data = anno_coop, aes(x = Treatment, y = y_pos, label = label), size = 6, fontface = "bold", color = "black", vjust=0) +
    scale_fill_brewer(palette = "Set1") +
    my_theme +
    labs(title = "Community Cross-Feeding Potential (Filtered)",
         subtitle = "Mean ± SE. Different letters indicate significant differences (Tukey's HSD, p < 0.05)",
         x = "Treatment Groups", y = "Total Cooperative Flux (Weight Sum)") +
    theme(legend.position = "none")
  
  p_ratio <- ggplot(eco_replicates, aes(x = Treatment, y = CC_Ratio, group = 1)) +
    stat_summary(fun = mean, geom = "line", color = "grey70", linetype = "dashed", linewidth = 0.8, alpha = 0.8) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15, color = "black", linewidth = 0.7) +
    stat_summary(fun = mean, geom = "point", aes(fill = Treatment), shape = 21, size = 4.5, color = "black", stroke = 0.8) +
    geom_jitter(aes(fill=Treatment), width = 0.1, shape = 21, size = 2, color = "grey30", stroke = 0.4, alpha = 0.7) +
    geom_text(data = anno_ratio, aes(x = Treatment, y = y_pos, label = label), size = 6, fontface = "bold", color = "black", vjust=0) +
    scale_fill_brewer(palette = "Set1") +
    geom_hline(yintercept = 1.0, linetype = "dashed", color = "grey40", linewidth = 0.7) + 
    my_theme +
    labs(title = "Cooperation-to-Competition Balance Index (C/C Ratio)",
         subtitle = "Normalized to Syn. Different letters indicate significance (Tukey's HSD, p < 0.05)",
         x = "Treatment Groups", y = "Relative Synergy Index (Fold Change Ratio)") +
    theme(legend.position = "none")
  
  ggsave(paste0(dir_eco, "1_Niche_Overlap.pdf"), p_overlap, width=7, height=6, bg="white")
  ggsave(paste0(dir_eco, "2_CrossFeeding_Potential.pdf"), p_coop, width=7, height=6, bg="white")
  ggsave(paste0(dir_eco, "3_Eco_Balance_Ratio.pdf"), p_ratio, width=7, height=5, bg="white")
  
  cat("\n[SUCCESS] 第 6 阶段高级生态关系解析已完美执行！\n")
  
=====
  # 7. 组内核心菌株关键载体代谢物 (Group-Specific Key Currencies) 靶向打捞
=====
  cat(">>> [7/7] 正在启动处理组内特异性核心菌株关键代谢载体打捞流水线...\n")
  
  dir_currency <- paste0(dir_macro, "001/")
  if (!dir.exists(dir_currency)) dir.create(dir_currency, recursive = TRUE)
  
  TARGET_WHITE_LIST <- c("S.Pse._bin_10") 
  
  if(length(keystone_files) > 0) {
    all_keystones_raw <- lapply(keystone_files, fread) %>% bind_rows()
    
    group_mags_scores <- all_keystones_raw %>%
      group_by(Group, MAG_ID) %>%
      summarise(Group_Hub_Power = sum(Keystone_Score, na.rm = TRUE), .groups = "drop")
    
    top_central_mags <- group_mags_scores %>%
      group_by(Group) %>%
      slice_max(order_by = Group_Hub_Power, n = 10, with_ties = FALSE) %>%
      ungroup() %>%
      rename(Treatment = Group) %>%
      select(Treatment, MAG_ID)
    
    # 【优化 5】：严格过滤确保人工靶向菌确实在网络中存在，防止生成无关联数据框
    whitelist_mags <- group_mags_scores %>%
      rename(Treatment = Group) %>%
      filter(MAG_ID %in% TARGET_WHITE_LIST) %>%
      select(Treatment, MAG_ID)
    
    group_pure_keystones <- bind_rows(top_central_mags, whitelist_mags) %>% distinct()
    
    group_keystone_currency_raw <- actual_mean_fluxes %>%
      filter(MacroCategory %in% core_5_categories | MAG_ID %in% TARGET_WHITE_LIST) %>% 
      inner_join(group_pure_keystones, by = c("Treatment", "MAG_ID")) %>%
      mutate(
        Direction = ifelse(Net_Flux > 0, "Community Secretion (Hub Export)", "Community Uptake (Hub Import)"),
        Exchange_Val = abs(Net_Flux)
      )
    
    top_group_pure_currencies <- group_keystone_currency_raw %>%
      group_by(Treatment, MAG_ID, Direction) %>%
      slice_max(order_by = Exchange_Val, n = 10, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(Treatment = factor(Treatment, levels = group_names)) 
    
    fwrite(top_group_pure_currencies %>% select(-Exchange_Val), 
           paste0(dir_currency, "Group_Specific_Keystone.csv"))
    
    flagship_mets <- top_group_pure_currencies %>%
      group_by(Metabolite_Name) %>%
      summarise(Total_Traffic = sum(Gross_Intensity, na.rm=TRUE), .groups = "drop") %>%
      arrange(desc(Total_Traffic)) %>%
      head(60) %>%
      pull(Metabolite_Name)
    
    plot_bubble_df <- top_group_pure_currencies %>%
      filter(Metabolite_Name %in% flagship_mets)
    
    if(nrow(plot_bubble_df) > 0) {
      dyn_h <- max(8, length(unique(plot_bubble_df$Metabolite_Name)) * 0.3 + 4)
      
      p_bubble <- ggplot(plot_bubble_df, aes(x = MAG_ID, y = Metabolite_Name)) +
        geom_point(aes(size = Exchange_Val, fill = MacroCategory), shape = 21, color = "black", stroke = 0.4, alpha = 0.85) +
        scale_size_continuous(range = c(2, 9), name = "Net Flux Magnitude") +
        scale_fill_manual(values = c(setNames(brewer.pal(5, "Set2"), core_5_categories), "Other" = "grey80"), name = "Functional Category") +
        facet_grid(Direction ~ Treatment, scales = "free", space = "free") +
        my_theme +
        labs(title = "Group-Specific Key Metabolic Currencies Mediated by Local Keystone Taxa",
             subtitle = "Top 10 active metabolites tracked strictly within each treatment's central and whitelisted MAGs",
             x = "Local Keystone Taxa", y = "Rhizosphere Target Metabolites") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
          axis.text.y = element_text(face = "bold.italic", size = 9),
          strip.text = element_text(size = 11, face = "bold"),
          panel.spacing = unit(3, "mm"),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.margin = margin(t = 0, b = 0)
        )
      
      ggsave(paste0(dir_currency, "Group_Specific_Keystone.pdf"), p_bubble, width = 16, height = dyn_h, bg = "white")
      ggsave(paste0(dir_currency, "Group_Specific_Keystone.png"), p_bubble, width = 16, height = dyn_h, dpi = 300, bg = "white")
    }
  }
  
  cat("\n[SUCCESS] 第 7 阶段核心菌关键代谢质谱解析圆满收官！\n")
  
  
  
  
}












#####!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#####
##### Figure 5E ####



# 0. 环境初始化与路径配置


# 清理 R 全局环境中的所有对象，防止之前运行残留的旧变量或冲突函数干扰本次分析
rm(list = ls())

# 强制执行垃圾回收，主动释放被 R 占用但已不再使用的内存空间，防止处理大网络矩阵时内存溢出
gc()

# 载入核心数据处理与网络可视化所需的 R 包
library(tidyverse) # 包含 ggplot2、dplyr、tidyr 等，用于进行高效的数据清洗、操纵与基础绘图
library(igraph)    # 经典的复杂网络分析核心包，用于构建、计算和导出网络拓扑结构
library(ggraph)    # 基于 ggplot2 语法的网络可视化拓展包，提供专门的网络布局和连线渲染器
library(patchwork) # 现代化的多图拼接包，用于将四组网络图优雅地合并为单张大图并统一图例

# 设置工作目录至项目的本地绝对路径，确保后续所有相对路径的读取与写入都能正确对齐
setwd("D:/work_year/2025_post_doc_NWSUAF/4_小麦抗旱项目/5_宏基因组-RNAseq/4_metadata_analysis/")

#  输入与输出路径变量配置 
# 定义 MICOM 宏观模块分析输出的原始数据目录（输入路径）
data_DIR <- "./2_analysis_R/3_bin-path/12_gapseqCOBRApyMICOM/5_MICOM/3_Streamlined_Macro_Modules/"

# 定义核心筛选网络图与 Gephi 拓扑文件的输出存放目录（输出路径）
out_DIR_Main <- "./2_analysis_R/3_bin-path/12_gapseqCOBRApyMICOM/5_MICOM/4_core-net-5-10-5/"

# 自动防御机制：若检测到输出目标文件夹在本地不存在，则递归地创建该文件夹路径
if (!dir.exists(out_DIR_Main)) dir.create(out_DIR_Main, recursive = TRUE)



# 1. 基础数据读取与列名安全清洗


library(dplyr)

# 读取三大核心数据表（网络边、核心菌株属性、核心代谢物丰度/评分表）
net_1 <- read.csv(paste0(data_DIR, "All_Mapped_Organics/0_Network_Topology_Global_all-edges.csv"), header = TRUE)
Key_MAGs_1 <- read.csv(paste0(data_DIR, "All_Mapped_Organics/Keystone_Stats.csv"), header = TRUE)
key_Met_1 <- read.csv(paste0(data_DIR, "All_Mapped_Organics/Global_Core_Metabolites_All_Groups_Stats.csv"), header = TRUE)

# 读取更新后的重命名映射表 (直接赋值给 taxa_map_raw)
taxa_map_raw <- read.table("./2_analysis_R/3_bin-path/0_data/3_Taxon_Levels_Completed_99_renamed.tsv", 
                           sep = "\t", header = TRUE, stringsAsFactors = FALSE)


# 2. 节点属性注入：将分类学信息无缝合并到 MAGs 属性表


# 1. 核心菌株属性表更新：通过 new_bin_name 匹配，引入完整的界门纲目科属种信息
Key_MAGs_0 <- Key_MAGs_1 %>%
  # Key_MAGs_0 的 MAG_ID 已经是如 "str26_Bacillus" 的格式，与 new_bin_name 直接完美对接
  left_join(taxa_map_raw, by = c("MAG_ID" = "new_bin_name")) %>%
  # 严谨处理：若遇到极个别未能匹配上注释信息的 MAG，将其分类阶元设为 "Unclassified" 防报错
  mutate(across(c(Kingdom, Phylum, Class, Order, Family, Genus, Species), 
                ~ ifelse(is.na(.), "Unclassified", .)))

# 2. 网络边表赋值：直接沿用 net_0，因为 from 和 to 已是标准易读名
net_0 <- net_1

# 3. 代谢物属性表赋值：无 MAG ID 参与，直接沿用
key_Met_0 <- key_Met_1

cat("\n[SUCCESS] 分类学数据已成功注入 Key_MAGs！网络节点现已具备 Phylum/Class 等门级可视化追踪能力。\n")

# 您可以通过以下命令预览结果，确保 Phylum 等信息已经挂载
# head(Key_MAGs)


# 【稳健性清洗】：检查并统一各数据表中代表分组的列名（若存在小写的 'group'，则强行更正为标准大写的 'Group'）
if ("group" %in% colnames(net_0)) colnames(net_0)[colnames(net_0) == "group"] <- "Group"
if ("group" %in% colnames(Key_MAGs_0)) colnames(Key_MAGs_0)[colnames(Key_MAGs_0) == "Group"] <- "Group"
if ("group" %in% colnames(key_Met_0)) colnames(key_Met_0)[colnames(key_Met_0) == "group"] <- "Group"

# 自定义项目中靶向关注的核心菌株列表（根据先验知识或前期试验确定的关键 Bins）
# key_bin <- c("S.Pse._bin_10", "S.Sph._bin_5")

key_bin <- c("str56_Pseudomonas_E", "str57_Pseudomonas_E") 

# key_bin <- c("S.Pse._bin_10", "S.Sph._bin_5", "S.Sph._bin_21")
# key_bin <- c("S.Sph._bin_21")

# 明确定义四个处理组的英文名称及它们的逻辑呈现顺序（对应后续图表拼接和 Facet 的排列顺序）
group <- c("Syn", "CK", "S.Pse.", "S.Sph.")
group_names <- group  



# 2. 全局代谢物 Top 15 提取与全局色彩字典构建 (跨组一致)


# 明确声明两个关键数据表中代表“代谢物名称”的列名，以便后续进行动态匹配和过滤
met_col_key <- "Metabolite"          # 核心代谢物评分表中的列名
met_col_net <- "Dominant_Metabolite" # 网络边数据表中的列名

# 安全防御检测：若在对应的数据表中找不到指定的列名，直接抛出错误提示并中断运行，防止产生空值逻辑
if(!(met_col_key %in% colnames(key_Met_0))) stop("报错：key_Met_0 中找不到 Metabolite 列！")
if(!(met_col_net %in% colnames(net_0))) stop("报错：net_0 中找不到 Dominant_Metabolite 列！")

# 提取各处理组中排名前 15 的核心代谢物
top15_mets_df <- key_Met_0 %>%
  group_by(Group) %>% # 按处理组进行局部拆分计算
  mutate(Core_Metabolite_Score = as.numeric(Core_Metabolite_Score)) %>% # 强转为数值型，防止由于文本型排序导致 Top 15 筛选错误
  arrange(desc(Core_Metabolite_Score)) %>% # 按核心代谢物评分由高到低进行降序排列
  slice_head(n = 15) %>% # 严格抽取每个处理组的前 15 行数据
  ungroup() # 及时释放分组状态，避免影响后续的数据流操作

# 获取全组所有 Top 15 代谢物的非重复合集（去重），用于统一构建色彩字典
global_top_mets <- unique(top15_mets_df[[met_col_key]])
num_mets <- length(global_top_mets) # 计算合集内总共包含了多少种不同的代谢物

# 预设一个由 20 种高区分度、高对比度组成的离散调色板（剔除了在白色背景上不显色的浅黄、浅灰等颜色）
distinct_palette <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", # 核心高饱和5色 (红、蓝、绿、紫、橙)
  "#A65628", "#F781BF", "#1B9E77", "#D95F02", "#7570B3", # 稳健深色系5色 (棕、粉、青/蓝绿、深橙、灰紫)
  "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#1F78B4", # 高对比过渡5色 (洋红、草绿、金黄、暗黄、深宝蓝)
  "#33A02C", "#FB8072", "#80B1D3", "#FDB462", "#BC80BD"  # 辅助明亮色5色 (深绿、珊瑚红、浅灰蓝、杏黄、丁香紫)
)

# 动态配色扩展：若全局核心代谢物总数超出了预设颜色数，则通过线性插值算法平滑扩展颜色数量，否则直接截取
if (num_mets > length(distinct_palette)) {
  final_palette <- colorRampPalette(distinct_palette)(num_mets)
} else {
  final_palette <- distinct_palette[1:num_mets]
}

# 构建全局色彩对应字典（命名向量）：将代谢物名称与其专属颜色绑定。实现跨处理组时，“同一种代谢物永远呈相同颜色”
global_met_colors <- setNames(final_palette, global_top_mets)



# 3. 动态阈值解构与四大生态象限划分


# 计算各处理组内部的出度与介数中心性的上四分位数（即 75% 分位数），以此作为划分四大生态功能格点的黄金阈值
quadrant_cutoffs <- Key_MAGs_0 %>%
  group_by(Group) %>% # 按组拆分，保证各处理组的切分点符合该组自身的拓扑分布特征
  summarise(
    cutoff_out = quantile(Out_Degree, 0.75, na.rm = TRUE), # 计算各组出度的 75% 严苛分位数
    cutoff_bet = quantile(Betweenness, 0.75, na.rm = TRUE), # 计算各组介数中心性的 75% 严苛分位数
    .groups = "drop" # 计算完毕后自动解除分组
  )  

# 空间投影算法：根据双轴阈值碰撞，将所有 MAG 划分到对应的生态角色象限中
quadrant_plot_data <- Key_MAGs_0 %>%
  left_join(quadrant_cutoffs, by = "Group") %>% # 将计算得到的动态阈值合并回节点统计主表
  group_by(Group) %>%
  mutate(
    # 基于 case_when 进行多条件判断和象限标记
    Quadrant = case_when(
      Out_Degree >= cutoff_out & Betweenness >= cutoff_bet ~ "Q1: Keystone Hubs", # 双高：核心系统主导者
      Out_Degree < cutoff_out  & Betweenness >= cutoff_bet ~ "Q2: Intermediaries", # 供需窄、控制强：中继协调者
      Out_Degree < cutoff_out  & Betweenness < cutoff_bet  ~ "Q3: Peripheral",     # 双低：基础的边缘参与者
      Out_Degree >= cutoff_out & Betweenness < cutoff_bet  ~ "Q4: Local Sources"   # 供需广、控制弱：本地生产商
    )
  ) %>%
  group_by(Group, Quadrant) %>% # 在各组内的各象限内部进行局部计算
  # min_rank 对介数中心性进行降序排名，处理并列排位，筛选真正的头部节点 (合并 Q1 和 Q2)
  mutate(Q1_Rank = if_else(Quadrant %in% c("Q1: Keystone Hubs", "Q2: Intermediaries"), min_rank(desc(Betweenness)), NA_integer_)) %>%
  group_by(Group) %>%
  # 智能标签防重叠释放：属于 Q1 或 Q2 象限内且排名前 15 的绝对明星菌，会被赋予 Label，其余均留空
  #  mutate(Label = ifelse(Quadrant %in% c("Q1: Keystone Hubs", "Q2: Intermediaries") & Q1_Rank <= 15, as.character(MAG_ID), "")) %>%
  mutate(Label = ifelse(Quadrant %in% c("Q1: Keystone Hubs", "Q2: Intermediaries"), as.character(MAG_ID), "")) %>%
  
  ungroup() %>% # 安全释放全局分组状态，杜绝后续分析出现未定义行为
  mutate(Group = factor(Group, levels = group_names)) # 将分组强制锁定为自定义的因子顺序，锁定绘图 Facet 排布



# 🌟 3.5 全局尺度统一预扫描 (仅提取包含 Key_Bin 的子网络)


# 单独设置 key_bin 的边权重过滤阈值 (用户可在此自由调整)
weight_threshold_key <- 5

# 初始化全局扫描标量，用于收集和计算全局多组间的统一物理度量
all_weights_scanned <- c()       # 收集所有组过滤后幸存的边权重，用于拉齐 ggplot 图例边宽
all_degrees_scanned <- c()       # 收集所有组内节点的实际度数，用于拉齐 ggplot 图例点大小
global_all_nodes_scanned <- c()  # 深度合并：捕获跨所有处理组过滤后依然存在的唯一节点全集，构建坐标大字典

cat("\n>>> 正在进行全局拓扑预扫描以统一范围...\n")

for (g in group) {
  # 临时提取该组代谢物、核心点与并集种子节点
  current_top_mets <- top15_mets_df %>% filter(Group == g) %>% pull(!!sym(met_col_key))
  if (length(current_top_mets) == 0) next
  
  # 获取对应组的原始边表
  if ("Group" %in% colnames(net_0)) {
    group_edges_all <- net_0 %>% filter(Group == g)
  } else {
    group_edges_all <- net_0 %>% filter(group == g)
  }
  
  # 🌟 核心过滤逻辑：仅提取涉及 key_bin 的互作关系，并应用阈值及 Top15 代谢物过滤
  temp_edges <- group_edges_all %>%
    filter((from %in% key_bin) | (to %in% key_bin)) %>%
    filter(Total_Weight >= weight_threshold_key) %>%
    filter(!!sym(met_col_net) %in% current_top_mets) %>%
    distinct() %>%
    mutate(from = as.character(from), to = as.character(to)) %>%
    filter(!is.na(from) & !is.na(to)) %>%    # 剔除空节点对
    select(from, to, everything())            # 🔥 核心防线：确保前两列必须是 from 和 to
  
  if (nrow(temp_edges) == 0) next
  
  # 收集过滤后真实的边权重
  all_weights_scanned <- c(all_weights_scanned, as.numeric(temp_edges$Total_Weight))
  
  # 抓取当前子网中出现的所有唯一活跃节点
  temp_nodes_subnet <- unique(c(temp_edges$from, temp_edges$to, key_bin))
  
  # 将当前组过滤后存在的节点无缝并入全局节点大池
  global_all_nodes_scanned <- unique(c(global_all_nodes_scanned, temp_nodes_subnet))
  
  # 属性对齐与异常节点拓扑补齐 (替换为合并了象限信息的 quadrant_plot_data)
  sub_nodes_temp <- quadrant_plot_data %>% 
    filter(Group == g, MAG_ID %in% temp_nodes_subnet) %>%
    mutate(MAG_ID = as.character(MAG_ID)) # 强转字符型
  
  missing_nodes_temp <- setdiff(temp_nodes_subnet, sub_nodes_temp$MAG_ID)
  if (length(missing_nodes_temp) > 0) {
    extra_nodes_temp <- tibble(
      MAG_ID = as.character(missing_nodes_temp), Group = g, Quadrant = "Neighbor", 
      Out_Degree = NA_real_, Betweenness = NA_real_, Q1_Rank = NA_integer_, Label = ""
    )
    sub_nodes_temp <- bind_rows(sub_nodes_temp, extra_nodes_temp)
  }
  
  # 【报错防线】：强制点表第1列为 ID，并严格去重
  sub_nodes_temp <- sub_nodes_temp %>% 
    filter(!is.na(MAG_ID)) %>%
    select(MAG_ID, everything()) %>%          
    distinct(MAG_ID, .keep_all = TRUE)
  
  # 临时构建网络对象以计算当前的局域拓扑度数分布
  g_temp <- graph_from_data_frame(d = temp_edges, directed = TRUE, vertices = sub_nodes_temp)
  deg_temp <- degree(g_temp, mode = "all")
  all_degrees_scanned <- c(all_degrees_scanned, deg_temp)
}

# 锁定全局统一的上下限极值，用作绘图时的 limits 控制器（若无数据，则使用保底值）
global_weight_min <- if(length(all_weights_scanned) > 0) min(all_weights_scanned, na.rm = TRUE) else 0.4
global_weight_max <- if(length(all_weights_scanned) > 0) max(all_weights_scanned, na.rm = TRUE) else 1.5
global_degree_min <- if(length(all_degrees_scanned) > 0) min(all_degrees_scanned, na.rm = TRUE) else 2
global_degree_max <- if(length(all_degrees_scanned) > 0) max(all_degrees_scanned, na.rm = TRUE) else 4

cat(paste("  [成功] 预扫描完毕：全局 Weight 统一边界为 [", global_weight_min, "-", global_weight_max, "]\n"))
cat(paste("  [成功] 预扫描完毕：全局 Degree 统一边界为 [", global_degree_min, "-", global_degree_max, "]\n\n"))



# 🌟 3.8 全局统一同心圆绝对物理坐标系搭建 (根治坐标视觉偏移)

cat(">>> 正在计算全局唯一的固定同心圆坐标字典...\n")

# 强控内层种子顺序！直接把 key_bin 作为 inner_nodes，切断由扫描顺序导致的排位错乱。
# 确保重点菌株永远锚定在固定的角度位置！
global_all_nodes_scanned <- unique(c(key_bin, global_all_nodes_scanned))
global_inner_nodes <- key_bin 
global_outer_nodes <- setdiff(global_all_nodes_scanned, key_bin)

# 初始化全局坐标矩阵，行名严格绑定全局唯一的节点 ID。
global_coords <- matrix(0, nrow = length(global_all_nodes_scanned), ncol = 2)
rownames(global_coords) <- c(global_inner_nodes, global_outer_nodes)

# A. 计算全局内圆排布字典，设定半径为 R_inner = 1.0，实现对称等分夹角分布
n_inner_global <- length(global_inner_nodes)
if (n_inner_global > 0) {
  angles_inner <- seq(0, 2 * pi, length.out = n_inner_global + 1)[1:n_inner_global]
  for (i in seq_along(global_inner_nodes)) {
    node <- global_inner_nodes[i]
    global_coords[node, ] <- c(1.0 * cos(angles_inner[i]), 1.0 * sin(angles_inner[i]))
  }
}

# B. 计算全局外圆排布字典 (所有其余外围 Hubs与共有邻居)，设定半径为 R_outer = 3.0，形成绝佳视觉景深
n_outer_global <- length(global_outer_nodes)
if (n_outer_global > 0) {
  angles_outer <- seq(0, 2 * pi, length.out = n_outer_global + 1)[1:n_outer_global]
  for (j in seq_along(global_outer_nodes)) {
    node <- global_outer_nodes[j]
    global_coords[node, ] <- c(3.0 * cos(angles_outer[j]), 3.0 * sin(angles_outer[j]))
  }
}
cat("  [成功] 全局坐标字典建立完成，物理绝对位置已被强行锁定！\n\n")



# 4. 分组子网提取、绘图与图表收集


# 初始化一个空的列表，用于按顺序保存各组生成的 ggplot2/ggraph 网络图对象，供最后合并拼接
plot_list <- list()

# 启动分组网络构建与可视化的主循环
for (g in group) {
  cat(">>> 正在处理分组:", g, "...\n")
  
  # 提取当前处理组所特定的 Top 15 核心代谢物英文名称
  current_top_mets <- top15_mets_df %>% filter(Group == g) %>% pull(!!sym(met_col_key))
  
  # 容错控制：如果当前处理组没有任何代谢物评分数据，打印警告并安全地跳过当前循环
  if(length(current_top_mets) == 0) {
    cat("  [警告] 当前组缺乏代谢物评分数据，已跳过。\n")
    next
  }
  
  # 获取对应组的原始边表
  if ("Group" %in% colnames(net_0)) {
    group_edges_all <- net_0 %>% filter(Group == g)
  } else {
    group_edges_all <- net_0 %>% filter(group == g)
  }
  
  # 🌟 严格对应预扫描阶段：直接提取所有涉及 key_bin 且达标的边
  sub_edges <- group_edges_all %>%
    filter((from %in% key_bin) | (to %in% key_bin)) %>%
    filter(Total_Weight >= weight_threshold_key) %>%
    filter(!!sym(met_col_net) %in% current_top_mets) %>%
    distinct() %>%
    mutate(from = as.character(from), to = as.character(to)) %>%
    filter(!is.na(from) & !is.na(to)) %>%
    select(from, to, everything())
  
  # 容错防爆：若该组过滤后未保留任何核心共养边，打印警告并跳过，防止后续网络分析崩溃
  if (nrow(sub_edges) == 0) {
    cat("  [警告] 当前组在过滤后未检测到任何有效边，已自动跳过。\n")
    next
  }
  
  # 创建一个新的列 'EdgeColorVar'，将边的颜色映射变量格式化为标准的字符型代谢物名称
  sub_edges <- sub_edges %>% mutate(EdgeColorVar = as.character(!!sym(met_col_net)))
  
  # 将自定义 key_bin 强制合入节点大列表中，确保它们即使无连接也能稳站在中心内环
  all_nodes_in_subnet <- unique(c(sub_edges$from, sub_edges$to, key_bin))
  
  # 从主数据 quadrant_plot_data 中，提取当前子网所含节点的属性子表
  sub_nodes <- quadrant_plot_data %>% 
    filter(Group == g, MAG_ID %in% all_nodes_in_subnet) %>%
    mutate(MAG_ID = as.character(MAG_ID)) # 强转字符型
  
  # 自动拓扑补全：检查是否有邻居节点因缺失属性而未被包含在 sub_nodes 中
  # 🌟 修复 1：在自动拓扑补全时，为分类学字段安全兜底，避免产生大量 NA，保障后续正常导出
  missing_nodes <- setdiff(all_nodes_in_subnet, sub_nodes$MAG_ID)
  if (length(missing_nodes) > 0) {
    extra_nodes <- tibble(
      MAG_ID = as.character(missing_nodes), Group = g, Quadrant = "Neighbor",
      Out_Degree = NA_real_, Betweenness = NA_real_, Q1_Rank = NA_integer_, Label = "",
      Kingdom = "Unknown", Phylum = "Unknown", Class = "Unknown", 
      Order = "Unknown", Family = "Unknown", Genus = "Unknown", Species = "Unknown"
    )
    sub_nodes <- bind_rows(sub_nodes, extra_nodes) # 合并补齐
  }
  
  # 【绘图防线】：强制去重以确保唯一的 ID 被投喂给 igraph，且保证第一列为 ID
  sub_nodes <- sub_nodes %>% 
    filter(!is.na(MAG_ID)) %>%
    select(MAG_ID, everything()) %>%
    distinct(MAG_ID, .keep_all = TRUE)
  
  # 根据节点身份划分为 Key Bin 以及 四大象限角色
  sub_nodes <- sub_nodes %>%
    mutate(
      NodeType = case_when(
        MAG_ID %in% key_bin ~ "Key Bin",
        Quadrant == "Q1: Keystone Hubs" ~ "Q1: Keystone Hubs",
        Quadrant == "Q2: Intermediaries" ~ "Q2: Intermediaries",
        Quadrant == "Q3: Peripheral" ~ "Q3: Peripheral",
        Quadrant == "Q4: Local Sources" ~ "Q4: Local Sources",
        TRUE ~ "Neighbor Nodes" # 兜底逻辑
      ),
      # 确保靶向菌株 Key_bin 始终显示 Label 名字，防遗漏
      Label = ifelse(MAG_ID %in% key_bin, as.character(MAG_ID), Label)
    )
  
  #  GraphML 安全性清洗与边宽数据转换模块 
  node_df <- sub_nodes 
  edge_df <- sub_edges 
  
  # 强制转换输送权重 Total_Weight 为数值型，若该列不存在，则启动兼容性匹配并兜底赋值为 1
  if ("Total_Weight" %in% colnames(edge_df)) {
    edge_df <- edge_df %>% mutate(Total_Weight = as.numeric(Total_Weight))
  } else {
    weight_cols <- colnames(edge_df)[tolower(colnames(edge_df)) == "total_weight"]
    if (length(weight_cols) > 0) {
      edge_df[[weight_cols[1]]] <- as.numeric(edge_df[[weight_cols[1]]])
      edge_df <- edge_df %>% rename(Total_Weight = !!sym(weight_cols[1]))
    } else {
      edge_df$Total_Weight <- 1
    }
  }
  
  # 彻底清除 R 中的因子型与逻辑型列，一律转为纯字符型，规避 GraphML 的不兼容报错
  node_df <- node_df %>% mutate(across(where(is.factor), as.character), across(where(is.logical), as.character))
  edge_df <- edge_df %>% mutate(across(where(is.factor), as.character), across(where(is.logical), as.character))
  
  # 针对含有大量 NA 缺失值的 Q1_Rank 排名整型列进行安全字符化，防止导出崩溃
  if ("Q1_Rank" %in% colnames(node_df)) node_df <- node_df %>% mutate(Q1_Rank = as.character(Q1_Rank))
  
  # 将所有纯数值列中的 NA 缺失值自动填充为数值 0，确保底层 C 语言校验接口能正常通过
  node_df <- node_df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
  edge_df <- edge_df %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
  
  #  构建网络拓扑结构图对象 
  g_igraph <- graph_from_data_frame(d = edge_df, directed = TRUE, vertices = node_df)
  
  # 重新计算在当前过滤子网下各个节点的真实连接度数（Degree），并将其绑定回点属性中映射点大小
  V(g_igraph)$degree <- degree(g_igraph, mode = "all")
  

  # 🌟 坐标映射：从全局字典中精准提取绝对坐标 (缺失节点自动留白)

  node_names <- V(g_igraph)$name # 获取当前子网实际拥有的节点顺序名单
  
  # drop = FALSE 确保即使当前网络只有1个节点，它依然保留矩阵的二维结构而不会降维报错
  coords <- global_coords[node_names, , drop = FALSE]

  

  # 🌟 出版级双层同心圆网络图核心绘制模块

  
  # 1. 绑定提取好的绝对坐标并初始化 ggraph 绘图画板
  # 初始化网络图对象
  p_net <- ggraph(g_igraph,                # 传入的网络图数据对象（必须是 igraph 或 tbl_graph 格式）
                  layout = "manual",       # 指定布局方式为“手动布局”
                  x = coords[, 1],         # 手动指定所有节点的 X 坐标（从 coords 矩阵/数据框的第一列获取）
                  y = coords[, 2]) +       # 手动指定所有节点的 Y 坐标（从 coords 矩阵/数据框的第二列获取）
    
    # 2. 绘制具有优美弧度的有向连线（边图层）
    # geom_edge_fan 用于绘制边缘，当两个节点之间有多条连线时，它会像扇子一样将线展开避免重叠；单条线时则表现为平滑的弧线
    geom_edge_fan(
      # 映射边的美学属性 (Aesthetics)
      aes(color = EdgeColorVar,        # 边的颜色：根据数据中的 'EdgeColorVar' 变量进行映射分类
          edge_width = Total_Weight),  # 边的粗细：根据数据中的 'Total_Weight'（权重）变量决定粗细
      
      strength = 2.2,                  # 弧度强度：数值越大，连线的弯曲程度（弧度）越明显
      alpha = 0.85,                    # 透明度：0.85 表示轻微透明（取值 0 到 1），有助于缓解线条密集交错时的视觉拥挤
      
      # 箭头设置：表明这是有向网络（如从 A 指向 B）
      arrow = arrow(length = unit(1.0, 'mm'),  # 箭头大小：长度设为 1.0 毫米
                    type = "open"),          # 箭头样式："closed" 表示实心闭合的三角形箭头
      
      # 节点避让设置（非常关键，防止线条和箭头插到节点图形的内部）
      start_cap = circle(1.3, 'mm'),   # 起点截断：在距离起点节点中心 1.3 毫米处开始画线（给起始节点留出空间）
      end_cap = circle(2.5, 'mm'),     # 终点截断：在距离终点节点中心 2.5 毫米处停止画线（留出更大空间，以免箭头被目标节点覆盖）
      
      show.legend = TRUE               # 图例设置：强制显示边属性（颜色和粗细）的图例
    ) +
    # 后续可以继续 + geom_node_point() 等添加节点图层...
    
    
    
    
    # 3. 绘制节点散点，映射象限分类颜色
    # 🌟 修复 2：将节点大小 size = degree 移入 aes 映射中，使其响应前面的预扫描
    # geom_node_point(aes(color = NodeType, size = degree), alpha = 0.95) +
    
    geom_node_point(aes(color = NodeType, size = 1.8), alpha = 0.95) +
    
    
    
    
    
    # 4. 绘制节点文本标签 (利用 ifelse 判断，隐藏空标签，避免多余遮挡)
    geom_node_text(aes(label = ifelse(Label == "", NA_character_, Label)), repel = TRUE, size = 2.5, fontface = "bold", max.overlaps = 25) +
    
    # 5. 边颜色手动控制器
    scale_edge_color_manual(values = global_met_colors, name = "Top 15 Core Metabolites", drop = TRUE) +
    
    # 6. 边粗细（权重）标度映射 
    scale_edge_width_continuous(
      range = c(0.15, 1.3), 
      limits = c(global_weight_min, global_weight_max), 
      name = "Total Weight"
    ) + 
    
    # 🌟 修复 3：新增节点大小（Degree）的全局范围标度映射
    scale_size_continuous(
      range = c(1.5, 5), # 节点最小和最大视觉尺寸
      limits = c(global_degree_min, global_degree_max), 
      name = "Node Degree"
    ) +
    
    # 7. 节点颜色手动控制器 (联动四大象限与靶向菌株)
    scale_color_manual(
      values = c(
        "Key Bin" = "#1F77B4",             # 蓝色
        "Q1: Keystone Hubs" = "#E41A1C",   # 红色
        "Q2: Intermediaries" = "#FF7F00",  # 橙色
        "Q3: Peripheral" = "#4DAF4A",      # 绿色
        "Q4: Local Sources" = "#984EA3",   # 紫色
        "Neighbor Nodes" = "#C4C2C4"       # 浅灰色 (兜底)
      ),
      breaks = c("Key Bin", "Q1: Keystone Hubs", "Q2: Intermediaries", "Q3: Peripheral", "Q4: Local Sources", "Neighbor Nodes"), 
      name = "Node Role"
    ) +
    
    # 9. 应用极简网络图专用空白主题
    theme_void() + 
    
    # 🌟 10. 终极物理防护：强锁绝对坐标边界与画布长宽比！
    coord_fixed(xlim = c(-4, 4), ylim = c(-4, 4), expand = TRUE) +
    
    # 11. 设置子图顶置主标题
    labs(title = paste("Group:", g)) + 
    
    # 12. 自定义高级视觉排版微调参数
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5, margin = margin(t = 10, b = 5)),
      
      # 图例排布细节美化
      legend.position = "right",                               
      legend.title = element_text(face = "bold", size = 9), 
      legend.text = element_text(size = 7.5),              
      legend.key.size = unit(4, "mm")                      
    )
  # 将绘制完成的图形对象收集到列表中
  plot_list[[g]] <- p_net
  
  # 导出单组的高分辨率单张图
  pdf_file <- paste0(out_DIR_Main, "Network_Subnet_", g, ".pdf")
  png_file <- paste0(out_DIR_Main, "Network_Subnet_", g, ".png")
  ggsave(pdf_file, plot = p_net, width = 4.5, height = 3.5) 
  ggsave(png_file, plot = p_net, width = 4.5, height = 3.5, dpi = 300)
  
  # 导出 Gephi 软件专用的黄金格式：GraphML 格式
  graphml_file <- paste0(out_DIR_Main, "Gephi_Subnet_", g, ".graphml")
  write_graph(g_igraph, file = graphml_file, format = "graphml")
  
  # 备份导出 csv 节点表与边表
  # 🌟 修复 4：导出 Gephi 时，将 Total_Weight 重命名为 Gephi 的原生字段 Weight
  gephi_nodes <- as.data.frame(node_df) %>% rename(Id = MAG_ID) %>% mutate(Label = Id) # 这里导出 Gephi 依然使用原有 ID 作为 Label
  gephi_edges <- as.data.frame(edge_df) %>% rename(Source = from, Target = to, Weight = Total_Weight) %>% mutate(Type = "Directed")
  write.csv(gephi_nodes, paste0(out_DIR_Main, "Gephi_Nodes_", g, ".csv"), row.names = FALSE)
  write.csv(gephi_edges, paste0(out_DIR_Main, "Gephi_Edges_", g, ".csv"), row.names = FALSE)
}



# 5. 核心图表合并 (四合一) 并导出


cat("\n>>> 正在合并四组网络图，生成同心圆总览大图...\n")

# 如果 4 个处理组都成功画出了子网，拼接开始
if (length(plot_list) > 0) {
  # patchwork一键拼图，guides = 'collect' 自动化繁为简合并去重图例
  combined_plot <- wrap_plots(plot_list, ncol = 2) + 
    plot_layout(guides = 'collect') + 
    plot_annotation(
      title = "Global Co-feeding Networks (Filtered by Group's Top 15 Metabolites)", 
      theme = theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5, margin = margin(b = 20)), 
        plot.background = element_rect(fill = "white", color = NA) 
      )
    )
  
  # 导出大图文件（8x7，确保完美容纳内外同心圆和右侧公共图例）
  combined_pdf <- paste0(out_DIR_Main, "Network_Combined_AllGroups.pdf")
  combined_png <- paste0(out_DIR_Main, "Network_Combined_AllGroups.png")
  
  ggsave(combined_pdf, plot = combined_plot, width = 8, height = 7)
  ggsave(combined_png, plot = combined_plot, width = 8, height = 7, dpi = 300) 
  
  cat("  [成功] 尺度与范围完全统一的四组合并同心圆网络大图已成功导出！\n")
} else {
  cat("  [提示] 未能生成任何有效的子图，已跳过合并拼接步骤。\n")
}

cat("\n===========\n")
cat("🎉 所有分组同心圆网络分析、拓扑清洗与拼接大图已全部圆满完成！\n")
cat("===========\n")






####***************************************************####

#### Figure 6 ####


rm(list = ls())
gc()

packages <- c("ggplot2", "dplyr", "ggpubr", "rstatix")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}




# 2. 读取数据 (这里假设使用的是去除了空因子的最新数据文件)
data0 <- read.table("./data_drew.txt", header = TRUE, sep = "\t")

# 3. 数据预处理
# 将 group 列转换为因子，并强制将 "Syn" 设为基线
data0$group <- factor(data0$group, levels = c("Syn", "Syn_Pse.", "Syn_Sph."))
data0$new_name <- as.factor(data0$new_name)



plot_delta_traits <- function(df_raw, traits_list, output_prefix) {
  
  if (!dir.exists("plots")) dir.create("plots")
  
  # 1. 清理幽灵因子：防止提取子集后产生坐标轴空白位
  df_raw <- droplevels(df_raw) 
  
  # 2. 计算对照组 (Syn) 在各个品种中的均值，作为 0 刻度基准
  ctrl_means <- df_raw %>%
    filter(group == "Syn") %>%
    group_by(new_name) %>%
    summarise(across(all_of(traits_list), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  colnames(ctrl_means)[-1] <- paste0(colnames(ctrl_means)[-1], "_ctrl")
  
  # 3. 构建作图用的差值数据 (处理组 - Syn组均值)
  df_diff <- df_raw %>%
    filter(group != "Syn") %>% # 图中不再需要画出全为0的对照组柱子
    droplevels() %>%
    left_join(ctrl_means, by = "new_name")
  
  for (tr in traits_list) {
    df_diff[[tr]] <- df_diff[[tr]] - df_diff[[paste0(tr, "_ctrl")]]
  }
  
  # 4. 循环绘图
  for (trait in traits_list) {
    message(paste("Drawing Delta plot for:", trait, "-", output_prefix))
    
    formula_str <- paste(trait, "~ group")
    
    #  A. 严谨统计：使用原始数据计算显著性 (加入恒量防御) 
    stat_raw <- df_raw %>%
      group_by(new_name) %>%
      group_modify(~ {
        tryCatch({
          # 默认使用 Welch's t-test (var.equal = FALSE)
          rstatix::t_test(data = .x, formula = as.formula(formula_str), ref.group = "Syn")
        }, error = function(e) {
          # 若某组方差为0，静默返回空数据框，防止整个脚本崩溃
          return(data.frame()) 
        })
      }) %>%
      ungroup()
    
    # 【优化点1】：不在这里过滤 "ns"，保留完整的行以维持绘图时的分组占位
    if (nrow(stat_raw) > 0) {
      stat_raw <- stat_raw %>%
        rstatix::add_significance("p")
    }
    
    #  B. 坐标精确映射：计算星号悬浮位置 
    summary_diff <- df_diff %>%
      group_by(new_name, group) %>%
      summarise(
        mean_val = mean(.data[[trait]], na.rm = TRUE),
        se = sd(.data[[trait]], na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      ) %>%
      mutate(se = ifelse(is.na(se), 0, se)) 
    
    text_data <- data.frame()
    if (nrow(stat_raw) > 0) {
      text_data <- stat_raw %>%
        left_join(summary_diff, by = c("new_name" = "new_name", "group2" = "group"))
      
      # 更严密的偏移量(Offset)算法：提取实际数据的极值计算绘图空间
      max_y <- max(summary_diff$mean_val + summary_diff$se, na.rm = TRUE)
      min_y <- min(summary_diff$mean_val - summary_diff$se, na.rm = TRUE)
      y_range <- abs(max_y - min_y)
      if (is.infinite(y_range) || is.na(y_range) || y_range == 0) y_range <- 1
      
      offset <- y_range * 0.08 # 星号距离误差线顶端的缓冲高度
      
      text_data <- text_data %>%
        mutate(
          # 正值：星号在顶端往上加；负值：星号在底端往下减
          y_pos = ifelse(mean_val >= 0, mean_val + se + offset, mean_val - se - offset),
          # 【优化点2】：确保 group 是因子，且水平与柱状图一致
          group = factor(group2, levels = levels(df_diff$group)),
          # 【优化点3】：将不显著的标记改为空白，实现隐形占位
          p_label = ifelse(p.signif == "ns", "", p.signif)
        ) %>%
        filter(!is.na(y_pos))
    }
    

    bar_colors <- c("#06B8BF", "#FF933B") 
    
    p <- ggbarplot(
      data = df_diff,                    # 指定绘图所需的数据框
      x = "new_name",                    # X轴映射：通常为分类变量（如品种、基因型、样本名）
      y = trait,                         # Y轴映射：连续型的目标性状数值（此处应为一个变量名）
      fill = "group",                    # 填充颜色：根据 "group" 变量分组（如对照组、处理组），产生不同颜色的柱子
      
      # 统计与误差线设置
      add = "mean_se",                   # 核心功能：自动计算各组的均值 (mean) 和标准误 (SE)，并绘制误差线
      
      # 核心优化：通过 add.params 精确控制误差线的视觉细节
      add.params = list(
        width = 0.3,                     # width: 控制误差线两端“帽子”的宽度。一般 0.15-0.3 较美观，太宽会显得笨重
        linewidth = 0.2,                 # linewidth: 误差线的线条粗细。特意设细一点(0.2)，使其不抢柱子边框(0.5)的风头，突出层次感
        color = "black"                  # color: 统一设为纯黑，保证印刷和高分辨率下的锐利度
      ),
      
      # 柱体布局与外观优化
      position = position_dodge(0.8),    # 分组柱子并排摆放。0.8 控制了同簇柱体之间的紧凑程度（避免完全贴合或分得太开）
      color = "black",                   # 统一设置所有柱子的外边框为黑色，增加图形的“界线感”，符合学术规范
      linewidth = 0.25,                   # 柱子外边框的粗细（较粗，奠定图形骨架）
      width = 0.7,                       # 单个柱子的宽度。0.7 留出了适当的柱间留白，避免画面过于拥挤死板
      palette = bar_colors               # 调用外部自定义的颜色向量 (bar_colors)，为 fill 分配具体颜色
    ) + 
      
      # 辅助基准线设置
    # 添加一条 y = 0 的水平线，通常用于强调正负差异（如 Treatment - Syn 的差值）
    geom_hline(
      yintercept = 0,                  # 截距在 Y 轴 0 处
      linetype = "dashed",             # 线型设为虚线，以区别于实线的坐标轴和柱体边框
      color = "#FC3337",                # 颜色设为中度灰，降低对比度，避免喧宾夺主抢夺柱状图的视觉焦点
      linewidth = 0.5                  # 粗细适中，保证可见但不过于突兀
    ) +
      
      # 整体主题设置
    theme_bw() +                       # 应用黑白主题，自带白色背景和浅色网格线，是学术SCI论文最常用的底板之一
      
      # 主题细节深度定制
    # 进一步向高标准学术期刊的清爽排版靠拢
    theme(
      # 1. X轴文本格式
      axis.text.x = element_text(
        angle = 45,                    # 标签倾斜45度，防止名称过长导致横向重叠
        hjust = 1,                     # 水平右对齐 (1)：使倾斜后的文本末端恰好指向对应的刻度线
        vjust = 1,                     # 垂直顶部对齐 (1)：优化文本与坐标轴的间距
        size = 9,                      # 字号适中，适合双栏排版的单栏图
        color = "black"                # 强制全黑（theme_bw默认有些偏灰），提升阅读清晰度
      ),
      
      # 2. Y轴文本格式
      axis.text.y = element_text(
        size = 9, 
        color = "black"                # 同样强制 Y 轴刻度数字为纯黑色
      ),
      
      # 3. 坐标轴标题格式
      axis.title = element_text(
        size = 10,                     # 标题字号(11)略大于刻度字号(9)，突出层级
        face = "bold",                 # 字体加粗，增强视觉重量
        color = "black"
      ),
      
      # 4. 坐标轴刻度线 (ticks)
      # 明确刻度线的颜色和粗细，防止导出 PDF 后线条发灰模糊
      axis.ticks = element_line(
        color = "black", 
        linewidth = 0.5                # 与柱子的边框线条粗细 (0.5) 保持一致，维持整体协调
      ), 
      
      # 5. 图例设置
      legend.position = "top",         # 将图例移至图表上方，节省下方空间，通常能让图表显得更宽阔
      legend.title = element_blank(),  # 隐藏图例标题（通常看图例内容就知道是什么，无需赘述）
      legend.background = element_blank(), # 背景设为透明，去除自带的白底，使其更好融入背景
      
      # 6. 网格线清理（极简主义）
      # 保留 Y 轴主网格线（方便读者比对柱子高度），去除其它所有干扰线
      panel.grid.major.x = element_blank(), # 去除 X 轴方向的垂直主网格线
      panel.grid.minor = element_blank(),   # 去除 X、Y 轴所有的次级细分网格线，使背景极度干净
      
      # 7. 全局外边框
      # 给整个作图区加一个粗边框，像相框一样“压住”整个画面
      panel.border = element_rect(
        color = "black", 
        linewidth = 0.8                # 线宽 0.8，比内部所有线条都粗，明确划定绘图边界
      ) 
    ) +
      
      # 坐标轴标签重命名
    labs(
      x = "Variety (new_name)",        # 完善 X 轴信息，告知读者这是品种信息
      y = paste0(trait) # 利用 paste0 动态拼接 Y 轴标题：添加增量符号 Δ，以及计算公式说明
    ) +
      
      
      
      
      # 坐标轴范围留白
    scale_y_continuous(
      # expand 控制坐标轴两端向外的延伸量
      # mult = c(0.10, 0.10) 表示在 Y 轴实际数据最大值/最小值的基础上，上下各留出 10% 的空白空间。
      # 核心目的：避免最高的柱体或误差线顶到天花板，且为后续添加“显著性星号(p-value)”预留出充足的位置。
      expand = expansion(mult = c(0.10, 0.10)) 
    )
    
    
    
    #  D. 叠加独立星号 (根据正负值自动贴靠) 
    if (nrow(text_data) > 0) {
      p <- p + geom_text(
        data = text_data,
        aes(x = new_name, y = y_pos, label = p_label, group = group), # 使用带占位的 p_label
        position = position_dodge(width = 0.8), # 现在宽度 0.8 可以精确对齐柱子了
        vjust = 0.5,
        size = 3,
        color = "black"
      )
    }
    
    #  E. 保存结果 
    ggsave(filename = paste0("plots/", output_prefix, "_Delta_", trait, ".pdf"), plot = p, width = 5, height = 3.1)
    ggsave(filename = paste0("plots/", output_prefix, "_Delta_", trait, ".png"), plot = p, width = 5, height = 3.1, dpi = 300)
  }
}









