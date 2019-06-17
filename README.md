# ChangeImageGPSInfo
随意更改照片GPS位置信息

scala语言编写

## 使用说明

1. 需要将代码中的高德API key换为自己的key。

2. 原始照片需要有GPS信息，否则无法修改。

3. 在解析高德API返回的坐标信息时，依赖了 json4s库
    ```scala
    val json4sNative = "org.json4s" %% "json4s-native" % "3.2.11"
    ```

