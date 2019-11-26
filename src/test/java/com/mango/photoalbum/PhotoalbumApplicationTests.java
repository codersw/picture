package com.mango.photoalbum;

import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.pojo.PhotoAlbum;
import com.mango.photoalbum.service.PhotoAlbumService;
import com.mango.photoalbum.utils.OtsUtils;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Date;

@RunWith(SpringRunner.class)
@SpringBootTest
@Slf4j
public class PhotoalbumApplicationTests {

    @Autowired
    private OtsUtils ots;

    /**
     * 创建表格
     */
    @Test
    public void creatTable() {
        ots.creatTable(PhotoAlbum.class);
    }

    /**
     * 删除表格
     */
    @Test
    public void deleteTable() {
        ots.deleteTable(PhotoAlbum.class);
    }

    /**
     * 保存
     */
    @Test
    public void save() {
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .albumId("1231322")
                .cover("12313213")
                .createTime(new Date())
                .modifyTime(new Date())
                .shootLocation("asdasdasdasd")
                .isDel(IsDelEnum.FALSE.getValue())
                .title("sadasdd")
                .userId(131312)
                .build();
        ots.creatRow(photoAlbum);
    }
}
