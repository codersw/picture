package com.mango.photoalbum.service.impl;

import com.mango.photoalbum.enums.IsDelEnum;
import com.mango.photoalbum.model.PhotoAlbumCo;
import com.mango.photoalbum.model.PhotoAlbum;
import com.mango.photoalbum.service.PhotoAlbumService;
import com.mango.photoalbum.utils.CommonUtils;
import com.mango.photoalbum.utils.OtsUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;
import java.util.Date;
import java.util.List;

@Service
public class PhotoAlbumServiceImpl implements PhotoAlbumService {

    @Resource
    private OtsUtils ots;

    @Override
    public PhotoAlbum save(PhotoAlbumCo photoAlbumCo) {
        ots.creatTable(PhotoAlbum.class);
        PhotoAlbum photoAlbum = PhotoAlbum.builder()
                .albumId(photoAlbumCo.getAlbumId())
                .shootTime(photoAlbumCo.getShootTime())
                .userId(photoAlbumCo.getUserId())
                .title(photoAlbumCo.getTitle())
                .cover(photoAlbumCo.getCover())
                .shootLocation(photoAlbumCo.getShootLocation())
                .isDel(IsDelEnum.FALSE.getValue())
                .modifyTime(new Date())
                .createTime(new Date())
                .build();
        if(StringUtils.isBlank(photoAlbumCo.getAlbumId())){
            photoAlbum.setAlbumId(CommonUtils.UUID());
            ots.creatRow(photoAlbum);
        } else {
            ots.updataRow(photoAlbum);
        }
        return photoAlbum;
    }

    @Override
    public void delete(String albumId) {
        ots.updataRow(PhotoAlbum.builder()
                .albumId(albumId)
                .isDel(IsDelEnum.TRUE.getValue())
                .modifyTime(new Date())
                .build());
    }

    @Override
    public PhotoAlbum get(String albumId) {
        return ots.retrieveRow(PhotoAlbum.builder()
                .albumId(albumId).build());
    }

    @Override
    public List<PhotoAlbum> list() {
        return null;
    }
}
