package com.mango.photoalbum.service.impl;

import com.mango.photoalbum.constant.TableNameContant;
import com.mango.photoalbum.model.co.PhotoAlbumCo;
import com.mango.photoalbum.model.pojo.PhotoAlbum;
import com.mango.photoalbum.model.vo.PhotoAlbumVo;
import com.mango.photoalbum.service.PhotoAlbumService;
import com.mango.photoalbum.utils.OtsUtils;
import org.springframework.stereotype.Service;
import javax.annotation.Resource;
import java.util.List;

@Service
public class PhotoAlbumServiceImpl implements PhotoAlbumService {

    @Resource
    private OtsUtils ots;

    @Override
    public PhotoAlbumVo save(PhotoAlbumCo PhotoAlbumCo) {
        existTable();
        return null;
    }

    @Override
    public void delete(String albumId) {

    }

    @Override
    public PhotoAlbumVo get(String albumId) {
        return null;
    }

    @Override
    public List<PhotoAlbumVo> list() {
        return null;
    }

    private void existTable(){
        if(!ots.existTable(TableNameContant.PHOTOALBUM)){
            ots.creatTable(ots.toTableStore(TableNameContant.PHOTOALBUM, PhotoAlbum.class, "albumId"));
        }
    }
}
