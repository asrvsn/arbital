import React, { Component } from 'react'
import { connect } from 'react-redux'

import Subheader from 'material-ui/Subheader';
import {List, ListItem} from 'material-ui/List';

import AuthoredListItem from './AuthoredListItem';
import GetterHOC from '../hoc/GetterHOC'

const styles= {
  root: {
    height: '100%',
    width: '100%',
  }
}

const Feed = (props) => {
  const { items, router, claimsDirty, reloadData } = props

  if (claimsDirty) {
    reloadData()
  }

  return (
    <div style={styles.root}>
      <List>
       <Subheader>Feed</Subheader>
        { (items == []) ?
          <ListItem primaryText={"No items to show"} />
          :
          items.map(item => (
            <AuthoredListItem
              key={item.id}
              text={item.text}
              authorId={item.authorId}
              authorName={item.authorName}
              onTouchTap={() => router.push('/claims/' + item.id)}
            />
          ))
        }
      </List>
    </div>
  )
}

export default GetterHOC(
  connect(
    ({ claimsDirty }) => ({ claimsDirty }),
    (dispatch) => ({ dispatch })
  )(Feed),

  (props) => ([
    {
      path: '/',
      mapResponseToProps: (resp) => ({items: resp})
    }
  ])
)
