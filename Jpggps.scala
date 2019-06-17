import scala.io.Source
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._
import org.json4s._
import java.io.File
import java.io.FileInputStream
import scala.collection.mutable.ArrayBuffer
import java.io.FileOutputStream

object Jpggps {
    def main(args: Array[String]): Unit = {
        val city = "北京"
        val addr = "天安门"
        val file = "d:/test.jpg"
        setChinaGPS(city, addr, file)
        println("位置修改完成")
    }

    /*
     * 根据地理位置，修改照片坐标坐标
     */
    def setChinaGPS(city: String, addr: String, jpg: String) {
        val jp = new Jpggps(jpg)
        val map = jp.getGPSidx
        val lngidx = map.getOrElse("Lng_24", 0)
        val latidx = map.getOrElse("Lat_24", 0)

        if(lngidx * latidx == 0){
            println("获取不到原始文件的经纬度信息！")
            return
        }
        val (lng1, lng2, lng3, lat1, lat2, lat3) = getGPS(city, addr)
        val a1 = jp.int2Bytes(1)
        val a100 = jp.int2Bytes(100)
        val arr_lng = jp.int2Bytes(lng1)
            .union(a1)
            .union(jp.int2Bytes(lng2))
            .union(a1)
            .union(jp.int2Bytes(lng3))
            .union(a100)
        val arr_lat = jp.int2Bytes(lat1)
            .union(a1)
            .union(jp.int2Bytes(lat2))
            .union(a1)
            .union(jp.int2Bytes(lat3))
            .union(a100)
        val bs = jp.bytes
            .patch(lngidx, arr_lng, arr_lng.size)
            .patch(latidx, arr_lat, arr_lat.size)
        val file = new File(jpg+".gps.jpg")
        val out = new FileOutputStream(file)
        out.write(bs)
        out.close()
    }

    /*
     * 根据地理位置，获取高德经纬度坐标
     */
    def getGPS(city: String, addr: String) = {
        val myKey = "**********" //高德API Key
        val url = s"""https://restapi.amap.com/v3/geocode/geo?key=${myKey}&address=${addr}&city=${city}"""
        val resp = Source.fromURL(url, "utf-8").mkString

        val jValue = parse(resp)
        // println(pretty(render(jValue)))

        val loc = (jValue \ "geocodes").asInstanceOf[JArray].apply(0) \ "location"
        val str = loc.asInstanceOf[JString].s

        val lng = str.split(",")(0).toDouble
        val lat = str.split(",")(1).toDouble
        //println(lng)
        //println(lat)

        val lng1 = lng.toInt
        val lng2 = ((lng - lng1) * 60).toInt
        val lng3 = ((((lng - lng1) * 60 - lng2) * 60) * 100).toInt

        val lat1 = lat.toInt
        val lat2 = ((lat - lat1) * 60).toInt
        val lat3 = ((((lat - lat1) * 60 - lat2) * 60) * 100).toInt

        (lng1, lng2, lng3, lat1, lat2, lat3)
    }

}

/*
 * jpg照片类，一个实例代表一张照片
 * 里面有获取照片信息的相关方法
 */
class Jpggps private {
    var filename: String = null
    var bytes: Array[Byte] = null
    var index: Int = 0
    var bigEndian: Boolean = false //jpgExif元信息是否为大端字节序

    def this(filename: String) = {
        this()
        this.filename = filename
        read_bytes
        find_index
        big_or_little
    }

    /*
     * 将照片内容读取为字节数组
     */
    private def read_bytes() = {
        val file = new File(filename)
        val in = new FileInputStream(new File(filename))
        bytes = new Array[Byte](file.length.toInt)
        in.read(bytes)
        in.close()
    }

    /*
     * 获取Exif元信息偏移量位置
     */
    private def find_index() = {
        val arr1 = Array(0xff.toByte, 0xe1.toByte)
        val arr2 = Array(0x45.toByte, 0x78.toByte, 0x69.toByte, 0x66.toByte, 0x00.toByte, 0x00.toByte)

        val buff = new ArrayBuffer[Int]()
        var idxt = bytes.indexOfSlice(arr1)
        while (idxt != -1) {
            buff.append(idxt)
            idxt = bytes.indexOfSlice(arr1, idxt + 1)
        }
        val buff2 = new ArrayBuffer[Int]()
        var idxt2 = bytes.indexOfSlice(arr2)
        while (idxt2 != -1) {
            buff2.append(idxt2)
            idxt2 = bytes.indexOfSlice(arr2, idxt2 + 1)
        }

        val indexs = buff.filter(idx => buff2.contains(idx + 4)).toArray
        require(indexs.size == 1, s"找到Exif头部${indexs.size}次!")
        index = indexs(0) + 10
    }

    override def toString() = {
        s"jpggps(path:${filename}, index:${index}, bigEndian:${bigEndian})"
    }

    /*
     * 判断Exif元信息字节序
     */
    private def big_or_little() {
        if (bytes(index) == 0x4d.toByte) {
            bigEndian = true
        } else if (bytes(index) == 0x49.toByte) {
            bigEndian = false
        } else {
            throw new IllegalArgumentException("字节序标识字符串解析错误！")
        }
    }

    /*
     * 获取照片GPS信息
     */
    def getGPSidx() = {
        val b8 = bytes.slice(index + 8, index + 10)
        val num = bytes2Int(b8)

        val arr = (0 until num).filter {
            i =>
                val idx = index + 10 + 12 * i
                bytes2Int(bytes.slice(idx, idx + 2)) == 0x8825
        }
        var map = scala.collection.mutable.Map[String, Int]()
        if (arr.size == 0) {
            println("没有GPS信息")
        } else {
            val idx = index + 10 + 12 * arr(0)
            val gps_index = bytes2Int(bytes.slice(idx + 10, idx + 12)) + index
            val count = bytes2Int(bytes.slice(gps_index, gps_index + 2))

            (0 until count).foreach {
                i =>
                    val idx = gps_index + 2 + 12 * i
                    val arr = bytes.slice(idx, idx + 2)
                    bytes2Int(arr) match {
                        case 0x0001 => map.put("N_S_2", idx + 8)
                        case 0x0002 => map.put("Lat_24", bytes2Int(bytes.slice(idx + 8, idx + 12)) + index)
                        case 0x0003 => map.put("E_W_2", idx + 8)
                        case 0x0004 => map.put("Lng_24", bytes2Int(bytes.slice(idx + 8, idx + 12)) + index)
                        case 0x0005 => map.put("0Up_1Down_1", idx + 8)
                        case 0x0006 => map.put("High_8", bytes2Int(bytes.slice(idx + 8, idx + 12)) + index)
                        case 0x0007 => map.put("UTC_24", bytes2Int(bytes.slice(idx + 8, idx + 12)) + index)
                        case 0x001d => map.put("Date_11", bytes2Int(bytes.slice(idx + 8, idx + 12)) + index)
                        case _ => None
                    }
            }
        }
        map
    }

    def bytes2Int(arr: Array[Byte]) = {
        require(arr != null && arr.size <= 4, "bytes2Int: 参数字不能为null且字节数量只能为0-4")

        var res = 0
        var arrt = arr
        if (bigEndian) {
            arrt = arr.reverse
        }

        var sca = 1
        arrt.foreach {
            b =>
                res += (b & 0xff) * sca
                sca *= 256
        }
        res
    }

    def int2Bytes(i: Int) = {
        var arr = new Array[Byte](4)
        arr(0) = (i & 0xff).toByte
        arr(1) = ((i >>> 8) & 0xff).toByte
        arr(2) = ((i >>> 16) & 0xff).toByte
        arr(3) = ((i >>> 24) & 0xff).toByte

        var res = arr
        if (bigEndian) {
            res = arr.reverse
        }
        res
    }
}