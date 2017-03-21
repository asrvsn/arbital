import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardText, CardTitle} from 'material-ui/Card';
import FlatButton from 'material-ui/FlatButton';
import Divider from 'material-ui/Divider';
import {List, ListItem} from 'material-ui/List';

import ClaimListItem from '../items/ClaimListItem';
import GetterHOC from '../hoc/GetterHOC';

const styles = {
  chip: {
    margin: 4,
  },
  row: {
    display: 'flex',
    flexDirection: 'row',
    justifyContent: 'space-between'
  }
}

const Argument = (props) => {
  const { page, session, router } = props
  const { arg, claims } = page

  const isMyArgument = session.user.id === arg.authorId

  return (
    <Card>
      <div style={styles.row}>
        <CardTitle
          title={arg.text}
          subtitle={arg.authorName}
        />
        { isMyArgument &&
          <CardActions>
            <FlatButton label="Update" />
            <FlatButton label="Delete" />
          </CardActions>
        }
      </div>

      <Divider />

      <CardHeader subtitle="Supporting claims" />
      <CardText>
        <List>
          { claims.map(claim =>
              <ClaimListItem
                key={claim.id}
                claim={claim}
                router={router}
              />
            )
          }
        </List>
      </CardText>
    </Card>
  )
}

export default GetterHOC(
  Argument,
  (props) => ({
    ARGUMENT: {
      path: props.location.pathname + '/page',
      mapResponseToProps: (resp) => ({page: resp})
    }
  })
)
