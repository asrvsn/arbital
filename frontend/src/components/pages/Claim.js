import React, { Component, PropTypes } from 'react'

import {Card, CardActions, CardHeader, CardMedia, CardTitle, CardText} from 'material-ui/Card';
import {List, ListItem} from 'material-ui/List';
import FlatButton from 'material-ui/FlatButton';
import {Tabs, Tab} from 'material-ui/Tabs'
import RaisedButton from 'material-ui/RaisedButton';
import Subheader from 'material-ui/Subheader';
import Divider from 'material-ui/Divider';

import ArgumentListItem from '../items/ArgumentListItem';
import GetterHOC from '../hoc/GetterHOC';

const styles = {
  row1: {
    display: 'flex',
    flexDirection: 'row',
    justifyContent: 'space-between'
  },
  row2: {
    display: 'flex',
    flexDirection: 'row'
  },
  row2item: {
    flexGrow: 1,
    flexBasis: 0
  },
}

class Claim extends Component {

  constructor(props) {
    super(props)
    this.state = {
    }
  }

  render() {
    const { page, router, session } = this.props
    const { claim, argsFor, argsAgainst } = page

    const getArgsList = (args) => (
      <List>
        { (args.length > 0) ?
            args.map(arg =>
              <ArgumentListItem
                key={arg.id}
                argument={arg}
                router={router}
              />
            )
          :
            <ListItem primaryText="(nothing)" />
        }
      </List>
    )

    const isMyClaim = session.user.id === claim.authorId

    return (
      <Card>
        <div style={styles.row1}>
          <CardTitle
            title={claim.text}
            subtitle={`${claim.authorName} created on ${claim.creationDate}`}
          />
          { isMyClaim &&
            <CardActions>
              <RaisedButton label="Update" primary={true} />
              <RaisedButton label="Delete" secondary={true} />
            </CardActions>
          }
        </div>

        <Divider />

        <CardText>
          <div style={styles.row2}>
            <div style={styles.row2item}>
              <Subheader>{`Arguments for (${argsFor.length})`}</Subheader>
              { getArgsList(argsFor) }
            </div>
            <div style={styles.row2item}>
              <Subheader>{`Arguments against (${argsAgainst.length})`}</Subheader>
              { getArgsList(argsAgainst) }
            </div>
          </div>
        </CardText>

      </Card>
    )
  }
}

export default GetterHOC(
  Claim,
  (props) => ({
    CLAIM: {
      path: props.location.pathname + '/page',
      mapResponseToProps: (resp) => ({page: resp})
    }
  })
)
